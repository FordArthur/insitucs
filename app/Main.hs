module Main where

import Data.List (isPrefixOf, elemIndex)
import Control.Monad.State (evalState, execState, runState, State, state, get, modify)
import Parser (buildAST)
import Types (Defs, InsiType(..), toValueOrNullT)

derefer :: [(InsiType, InsiType)] -> InsiType -> InsiType
derefer binds (Idn var)
    | var `elem` opers = Idn var
    | otherwise = derefer binds val
    where Just val = lookUpOneOf [Idn var, Idn $ '\"' : var ++ "\""] binds
            where lookUpOneOf _ [] = Nothing
                  lookUpOneOf xs ((k, v):ds)
                      | k `elem` xs = Just v
                      | otherwise   = lookUpOneOf xs ds
derefer binds (Vec v) = Vec . substitute binds $ v
derefer binds (Exp e) = evalState (apply $ Exp e) binds
derefer binds (Clo t f) = Clo t f
derefer _ val = val

substitute :: [(InsiType, InsiType)] -> [InsiType] -> [InsiType]
substitute argLookUp = map (derefer argLookUp)
opers :: [String]
opers = ["+", "if"]

(>>>==) :: Defs -> [InsiType] -> State Defs InsiType
s' >>>== [f] = state $ \s -> runState (apply f) (s ++ s')
s >>>== (f:fs) = (s' ++ s) >>>== fs
     where s' = execState (apply f) s

apply ::  InsiType -> State Defs InsiType

apply (Binds b) = modify (++ b) >> return a
    where (_, a) = last b
apply (Exp [Num n, Vec xs]) = return . toValueOrNullT id $ floor n `at` xs
    where at _ [] = Nothing
          at 0 (x:_) =  Just x
          at n (x:xs) = at (n - 1) xs
apply (Exp [Num n, Str cs]) = return . toValueOrNullT (Str . (: "")) $ floor n `at` cs
    where at _ [] = Nothing
          at 0 (x:_) =  Just x
          at n (x:xs) = at (n - 1) xs
apply (Exp [Num n, i]) = get >>= (\s -> apply . Exp $ [Num n, derefer s i])
apply (Exp [Vec xs, thing]) = return $ if thing `elem` xs then thing else Null

apply (Exp [Dict ds, key]) = return value
    where value = toValueOrNullT id $ lookup key ds
apply (Exp (Clo "lam" (cloArgs, lam):args)) = get >>= exprs >>= apply
     where exprs s = return . Exp $ substitute (zip cloArgs args ++ s) lam
apply (Exp (Clo "part" (cloArgs, lam):args)) = get >>= exprs >>= apply
    where exprs s = return . Exp . (++ args) $ substitute (zip cloArgs args ++ s) lam
apply (Exp (Clo "fun" (cloArgs, func):args)) = get >>= exprs
    where exprs s = (zip cloArgs args ++ s) >>>== func
          getExp (Exp e) = e

apply (Exp (Idn "+":args)) = get >>= (\s -> return . Num . sum $ map (fromNum . derefer s) args)
    where fromNum (Num n) = n -- temporal solution, typechecking will solve this

apply (Exp (Idn "if":Bool p:a:b:_))
    | p         = apply a
    | otherwise = apply b

apply (Exp (Idn i:args)) = get >>= (\s -> apply . Exp $ derefer s (Idn i) : args)
apply val = return val

recursiveFeed :: (Defs -> InsiType -> (InsiType, Defs)) -> Defs -> [InsiType] -> [(InsiType, Defs)]
recursiveFeed _ _ [] = []
recursiveFeed f s (x:xs) = (a', s') : recursiveFeed f (s ++ s') xs
    where (a', s') = f s x

-- "repl" mode
main :: IO ()
main = do
    i <- getLine
    putStrLn $ case buildAST i of
        (Right xs) -> show $ recursiveFeed (\s a -> runState (apply a) s) [] xs
        (Left e)  -> show e 
    main