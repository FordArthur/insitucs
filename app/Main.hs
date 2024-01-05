module Main where

import Data.Char (isDigit)
import Data.List (isPrefixOf, elemIndex)
import Data.Data
import Control.Monad.State
    ( (>=>),
      MonadTrans(lift),
      MonadState(get),
      gets,
      modify,
      evalStateT,
      execStateT,
      StateT (runStateT) )
import Parser (buildAST)
import Types (RunError(..), Label, Defs, InsiType(..), IsLazy, toValueOrNullT, opers, builtIn, typeCheck, constrInString)

derefer :: Defs -> InsiType -> StateT Defs (Either RunError) InsiType
derefer binds i@(Idn _ var)
    | any (\(o, _) -> o == var) opers = return i
    | otherwise = lift val >>= derefer binds
    where val = lookif (\(Idn _ var') -> var' == var) binds 
            where lookif _ [] = Left $ OutOfScope i
                  lookif p ((k, v):xs)
                    | p k = Right v
                    | otherwise = lookif p xs
derefer binds (Vec v) = fmap Vec . substitute binds $ v
derefer binds (Exp e) = Exp <$> substitute binds e
derefer binds (Clo t l f) = return $ Clo t l f
derefer binds b@(Binds _) = eval b
derefer _ val = return val

check :: InsiType -> Either RunError InsiType
check (Exp (h@(Idn _ f'):args')) = typeCheck h  (todoLookup f' opers) args'
check (Exp (f':args'))           = typeCheck f' (todoLookup (constrInString f') builtIn) args'
check ix                         = Right ix

simplify :: InsiType -> [IsLazy] -> StateT Defs (Either RunError) InsiType
simplify (Exp (f:args)) lazy = Exp <$> (mapM (\(ix, lazy) -> if lazy then return ix else run ix) . ((f, False) :) . zip args) lazy
simplify ix        _    = return ix

run :: InsiType -> StateT Defs (Either RunError) InsiType
run f = get >>= (`derefer` f) >>= mkLazy >>= uncurry simplify >>= (lift . check) >>= eval
    where mkLazy exp@(Exp (Idn _ f:_)) = return (exp, map snd . fst . todoLookup f $ opers)
          mkLazy exp@(Exp (built:_))   = return (exp, map snd . fst . todoLookup (constrInString built) $ builtIn)
          mkLazy ix                    = return (ix , [])

substitute :: Defs -> [InsiType] -> StateT Defs (Either RunError) [InsiType]
substitute binds [f] = (:[]) <$> derefer binds f
substitute binds (f:fs) = (:) <$> f' <*> (lift (execStateT f' binds) >>= (\s' -> substitute (s' ++ binds) fs))
    where f' = derefer binds f

recursiveExec :: Defs -> [InsiType] -> Label -> Either RunError InsiType
recursiveExec s [f] l = evalStateT (run f) s
recursiveExec s (f:fs) l = s' >>= (\s' -> recursiveExec (s' ++ s) fs l)
    where s' = execStateT (run f) s

todoLookup :: String -> [(String, v)] -> v
todoLookup x xs = 
    case lookup x xs of
        Just x' -> x'
        Nothing -> error $ "IMPLEMENT " ++ x

eval ::  InsiType -> StateT Defs (Either RunError) InsiType

eval (Binds b) = modify (++ b) >> run a
    where (_, a) = last b

eval (Exp [Num n, Vec xs]) = return . toValueOrNullT id $ floor n `at` xs
    where at _ [] = Nothing
          at 0 (x:_) =  Just x
          at n (x:xs) = at (n - 1) xs
eval (Exp [Num n, Str cs]) = return . toValueOrNullT (Str . (: "")) $ floor n `at` cs
    where at _ [] = Nothing
          at 0 (x:_) =  Just x
          at n (x:xs) = at (n - 1) xs

eval (Exp [Vec xs, thing]) = return $ if thing `elem` xs then thing else Null

eval (Exp [Dict ds, key]) = return value
    where value = toValueOrNullT id $ lookup key ds

eval (Exp (Clo "lam" l (cloArgs, lam):args)) = get >>= exprs >>= eval
     where exprs s = Exp <$> substitute (zip cloArgs args ++ s) lam

eval (Exp (Clo "part" l (cloArgs, lam):args)) = get >>= exprs >>= eval
    where exprs s = Exp .  (++ args) <$> substitute (zip cloArgs args ++ s) lam
    
eval (Exp (Clo "fun" l (cloArgs, func):args)) = get >>= exprs
    where exprs s = lift $ recursiveExec (zip cloArgs args ++ s) func l

eval (Exp (Idn _ "+":args)) = return . Num . sum . map fromNum $ args
    where fromNum (Num n) = n

eval (Exp [Idn _ "if", p, a])
    | useBool p && (p /= Null) = run a
    | otherwise                = return Null
    where useBool (Bool p) = p
          useBool _        = True

eval (Exp [Idn _ "if", p, a, b])
    | useBool p && (p /= Null) = run a
    | otherwise                = run b
    where useBool (Bool p) = p
          useBool _        = True

eval (Exp (Idn _ "=":a:rgs)) = return . Bool $ all (a ==) rgs

eval val = return val

-- "repl" mode
main :: IO ()
main = do
    i <- getLine
    putStrLn $ case buildAST i of
        (Right xs) -> show $ recursiveExec [] xs (Nothing, Nothing)
        (Left e)  -> show e 
    main
