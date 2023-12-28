module Main where

{-# LANGUAGE DeriveDataTypeable #-}

import Data.Char (isDigit)
import Data.List (isPrefixOf, elemIndex)
import Control.Monad.State
import Parser (buildAST)
import Types (RunError(..), Label, Defs, InsiType(..), toValueOrNullT, opers, typeCheck)

derefer :: Defs -> InsiType -> Either RunError InsiType
derefer binds i@(Idn p var)
    | any (\(o, _) -> o == var) opers = Right i
    | otherwise = val >>= derefer binds
    where val = lookif (\(Idn _ var') -> var' == var) binds 
            where lookif _ [] = Left $ OutOfScope i
                  lookif p ((k, v):xs)
                    | p k = Right v
                    | otherwise = lookif p xs
derefer binds (Vec v) = fmap Vec . substitute binds $ v
derefer binds e@(Exp _) = evalStateT (eval e) binds
derefer binds (Clo t l f) = Right $ Clo t l f
derefer _ val = Right val


substitute :: Defs -> [InsiType] -> Either RunError [InsiType]
substitute argLookUp = traverse (derefer argLookUp)

recursiveExec :: Defs -> [InsiType] -> Label -> Either RunError InsiType
recursiveExec s [f] l = evalStateT (eval f) s
    where cleanScope (p, q) = (p, filter (not . retiring l) q)
          retiring l (Idn l' _, _) = l == l'
          retiring _ _             = False
recursiveExec s (f:fs) l = s' >>= (\s' -> recursiveExec (s' ++ s) fs l)
    where s' = execStateT (eval f) s

todoLookup :: String -> [(String, v)] -> v
todoLookup x xs = 
    case lookup x xs of
        Just x' -> x'
        Nothing -> error $ "IMPLEMENT " ++ x

eval ::  InsiType -> StateT Defs (Either RunError) InsiType

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
eval (Exp [n@(Num _), i]) = get >>= (\s -> either (lift . Left) (\i' -> eval (Exp [n, i'])) (derefer s i))
eval (Exp [Vec xs, thing]) = return $ if thing `elem` xs then thing else Null

eval (Exp [Dict ds, key]) = return value
    where value = toValueOrNullT id $ lookup key ds
eval (Exp (Clo "lam" l (cloArgs, lam):args)) = get >>= exprs >>= eval
     where exprs s = lift $ Exp <$> substitute (zip cloArgs args ++ s) lam
eval (Exp (Clo "part" l (cloArgs, lam):args)) = get >>= exprs >>= eval
    where exprs s = lift $ Exp .  (++ args) <$> substitute (zip cloArgs args ++ s) lam
eval (Exp (Clo "fun" l (cloArgs, func):args)) = get >>= exprs
    where exprs s = lift $ recursiveExec (zip cloArgs args ++ s) func l

eval (Exp (Idn _ "+":args)) = do
        s <- get
        args' <- lift $ traverse (derefer s) args
        lift $ typeCheck (todoLookup "+" opers) args'
        lift $ Right . Num . sum . map fromNum $ args'
    where fromNum (Num n) = n

eval (Exp (Idn _ "if":Bool p:a:b:_))
    | p         = eval a
    | otherwise = eval b

eval (Exp (i@(Idn _ _):args)) = get >>= (\s -> lift (Exp . (: args) <$> derefer s i) >>= eval)
eval val = return val

-- "repl" modesdsds
main :: IO ()
main = do
    i <- getLine
    putStrLn $ case buildAST i of
        (Right xs) -> show $ recursiveExec [] xs (Nothing, Nothing)
        (Left e)  -> show e 
    main