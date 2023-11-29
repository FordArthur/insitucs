module Main where

import Data.Char (isDigit)
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
derefer binds (Exp e) = evalState (eval $ Exp e) binds
derefer binds (Clo t l f) = Clo t l f -- force f to be strict
derefer _ val = val

substitute :: Defs -> [InsiType] -> [InsiType]
substitute argLookUp = map (derefer argLookUp)

opers :: [String]
opers = ["+", "if"]

(>>>==) :: Defs -> [InsiType] -> State Defs InsiType
s' >>>== [f] = state (\s -> cleanLets $ runState (eval f) (s ++ s'))
    where cleanLets (a, s) = (a, filter (\(Idn i, _) -> not ("\"" `isPrefixOf` i || (isDigit . head) i)) s)
s >>>== (f:fs) = (s' ++ s) >>>== fs
     where s' = execState (eval f) s


recursiveExec s [f] l = state (\s' -> cleanScope $ runState (eval f) (s' ++ s))
    where cleanScope (p, q) k = (p, filter (not . (\(n, b) -> )) q)
recursiveExec s (f:fs) l = recursiveExec (s' ++ s) fs l
    where s' = execState (eval f) s


eval ::  InsiType -> State Defs InsiType

eval (Binds b) = modify (++ b) >> return a
    where (_, a) = last b
eval (Exp [Num n, Vec xs]) = return . toValueOrNullT id $ floor n `at` xs
    where at _ [] = Nothing
          at 0 (x:_) =  Just x
          at n (x:xs) = at (n - 1) xs
eval (Exp [Num n, Str cs]) = return . toValueOrNullT (Str . (: "")) $ floor n `at` cs
    where at _ [] = Nothing
          at 0 (x:_) =  Just x
          at n (x:xs) = at (n - 1) xs
eval (Exp [Num n, i]) = get >>= (\s -> eval . Exp $ [Num n, derefer s i])
eval (Exp [Vec xs, thing]) = return $ if thing `elem` xs then thing else Null

eval (Exp [Dict ds, key]) = return value
    where value = toValueOrNullT id $ lookup key ds
eval (Exp (Clo "lam" l (cloArgs, lam):args)) = get >>= exprs >>= eval
     where exprs s = return . Exp $ substitute (zip cloArgs args ++ s) lam
eval (Exp (Clo "part" l (cloArgs, lam):args)) = get >>= exprs >>= eval
    where exprs s = return . Exp . (++ args) $ substitute (zip cloArgs args ++ s) lam
eval (Exp (Clo "fun" l (cloArgs, func):args)) = get >>= exprs
    where exprs s = (zip cloArgs args ++ s) >>>== func
          getExp (Exp e) = e

eval (Exp (Idn "+":args)) = get >>= (\s -> return . Num . sum $ map (fromNum . derefer s) args)
    where fromNum (Num n) = n -- temporal solution, typechecking will solve this

eval (Exp (Idn "if":Bool p:a:b:_))
    | p         = eval a
    | otherwise = eval b

eval (Exp (Idn i:args)) = get >>= (\s -> eval . Exp $ derefer s (Idn i) : args)
eval val = return val

recursiveRun :: (Defs -> InsiType -> (InsiType, Defs)) -> Defs -> [InsiType] -> [(InsiType, Defs)]
recursiveRun _ _ [] = []
recursiveRun f s (x:xs) = (a', s') : recursiveRun f (s ++ s') xs
    where (a', s') = f s x

-- "repl" mode
main :: IO ()
main = do
    i <- getLine
    putStrLn $ case buildAST i of
        (Right xs) -> show $ recursiveRun (\s a -> runState (eval a) s) [] xs
        (Left e)  -> show e 
    main