module Main where

import Data.Char (isDigit)
import Data.List (isPrefixOf, elemIndex)
import Control.Monad.State (evalState, execState, runState, State, state, get, modify)
import Parser (buildAST)
import Types (Label, Defs, InsiType(..), toValueOrNullT)

derefer :: Defs -> InsiType -> InsiType
derefer binds i@(Idn p var)
    | var `elem` opers = i
    | otherwise = derefer binds val
    where Just val = lookif (\(Idn _ var') -> var' == var) binds
            where lookif _ [] = Nothing
                  lookif p ((k, v):xs)
                    | p k = Just v
                    | otherwise = lookif p xs
derefer binds (Vec v) = Vec . substitute binds $ v
derefer binds (Exp e) = evalState (eval $ Exp e) binds
derefer binds (Clo t l f) = Clo t l f
derefer _ val = val

substitute :: Defs -> [InsiType] -> [InsiType]
substitute argLookUp = map (derefer argLookUp)

opers :: [String]
opers = ["+", "if"]

recursiveExec :: Defs -> [InsiType] -> Label -> State Defs InsiType
recursiveExec s [f] l = state (\_ -> cleanScope $ runState (eval f) s)
    where cleanScope (p, q) = (p, filter (not . retiring l) q)
          retiring l (Idn l' _, _) = l == l'
          retiring _ _             = False
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
    where exprs s = recursiveExec (zip cloArgs args ++ s) func l

eval (Exp (Idn _ "+":args)) = get >>= (\s -> return . Num . sum $ map (fromNum . derefer s) args)
    where fromNum (Num n) = n -- temporal solution, typechecking will solve this

eval (Exp (Idn _ "if":Bool p:a:b:_))
    | p         = eval a
    | otherwise = eval b

eval (Exp (Idn p i:args)) = get >>= (\s -> eval . Exp $ derefer s (Idn p i) : args)
eval val = return val

recursiveRun :: (Defs -> InsiType -> (InsiType, Defs)) -> Defs -> [InsiType] -> [(InsiType, Defs)]
recursiveRun _ _ [] = []
recursiveRun f s (x:xs) = (a', s') : recursiveRun f (s ++ s') xs
    where (a', s') = f s x

-- "repl" mode dsdssdsds
main :: IO ()
main = do
    i <- getLine
    putStrLn $ case buildAST i of
        (Right xs) -> show $ recursiveRun (\s a -> runState (eval a) s) [] xs
        (Left e)  -> show e 
    main