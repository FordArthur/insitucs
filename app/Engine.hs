module Engine where

import System.Exit ( ExitCode(..) )

import Data.Char (isDigit)
import Data.List (isPrefixOf, elemIndex, transpose)
import qualified Data.Map as Map
import Control.Monad (filterM, foldM)
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
import Types (RunError(..), Defs, InsiType(..), IsLazy, useBool, fromNum, fromList, toValueOrNullT, opers, builtIn, typeCheck, constrInString)

derefer :: Defs -> InsiType -> StateT Defs (Either RunError) InsiType
derefer binds i@(Idn var)
    | any (\(o, _) -> o == var) opers = return i
    | otherwise = lift val >>= derefer binds
    where val = throwOutOfScope $ Map.lookup i binds
            where throwOutOfScope (Just x) = Right x
                  throwOutOfScope Nothing  = Left $ OutOfScope i

derefer binds (Vec v) = fmap Vec . substitute binds $ v
derefer binds (Exp e) = Exp <$> substitute binds e
derefer binds (Clo l f) = return $ Clo l f
derefer binds b@(Binds{}) = eval b
derefer _ val = return val

check :: InsiType -> Either RunError InsiType
check (Exp (h@(Idn f'):args')) = typeCheck h  (todoLookup f' opers) args'
check (Exp (f':args'))         = typeCheck f' (todoLookup (constrInString f') builtIn) args'
check ix                       = Right ix

simplify :: InsiType -> [IsLazy] -> StateT Defs (Either RunError) InsiType
simplify (Exp (f:args)) lazy = Exp <$> (mapM (\(ix, lazy) -> if lazy then return ix else run ix) . ((f, False) :) . zip args) lazy
simplify ix        _    = return ix

run :: InsiType -> StateT Defs (Either RunError) InsiType
run f = mkLazy f >>= uncurry simplify >>= (lift . check) >>= eval
    where mkLazy exp@(Exp (Idn f:_)) = return (exp, map snd . fst . todoLookup f $ opers)
          mkLazy exp@(Exp (built:_)) = return (exp, map snd . fst . todoLookup (constrInString built) $ builtIn)
          mkLazy ix                  = return (ix , [])

substitute :: Defs -> [InsiType] -> StateT Defs (Either RunError) [InsiType]
substitute binds [] = return []
substitute binds (f:fs) = (:) <$> f' <*> (lift (execStateT f' binds) >>= (\s' -> substitute (s' `Map.union` binds) fs))
    where f' = derefer binds f

recursiveExec :: Defs -> [InsiType] -> Either RunError InsiType
recursiveExec s [f] = evalStateT (get >>= (`derefer` f) >>= run) s
recursiveExec s (f:fs) = s' >>= (\s' -> recursiveExec (s' `Map.union` s) fs)
    where s' = execStateT (get >>= (`derefer` f) >>= run) s

todoLookup :: String -> [(String, v)] -> v
todoLookup x xs = 
    case lookup x xs of
        Just x' -> x'
        Nothing -> error $ "IMPLEMENT " ++ x

eval ::  InsiType -> StateT Defs (Either RunError) InsiType

eval (Binds _ l b) = modify (`Map.union` b) >> run l

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
    where value = toValueOrNullT id $ Map.lookup key ds

eval (Exp (Clo "lam" (cloArgs, lam):args)) = get >>= exprs >>= eval
     where exprs s = Exp <$> substitute (Map.fromList ((Idn "args", Vec args) : zip cloArgs (args ++ repeat Null)) `Map.union` s) lam

eval (Exp (Clo "part" (cloArgs, lam):args)) = get >>= exprs >>= eval
    where exprs s = Exp <$> substitute (Map.fromList ((Idn "args", Vec args) : zip cloArgs (args ++ repeat Null)) `Map.union` s) lam
    
eval (Exp (Clo "fun" (cloArgs, func):args)) = get >>= exprs
    where exprs s = lift $ recursiveExec (Map.fromList ((Idn "args", Vec args) : zip cloArgs (args ++ repeat Null)) `Map.union` s) func

eval (Exp [Idn "if", p, a])
    | useBool p && (p /= Null) = run a
    | otherwise                = return Null

eval (Exp [Idn "if", p, a, b])
    | useBool p && (p /= Null) = run a
    | otherwise                = run b

eval (Exp (Idn "=":a:rgs)) = return . Bool $ all (a ==) rgs

eval (Exp (Idn "and":args)) = return . Bool . all useBool $ args
eval (Exp (Idn "or":args))  = return . Bool . any useBool $ args

eval (Exp (Idn "+":args)) = return . Num . sum . map fromNum $ args
eval (Exp (Idn "-":a:rgs)) = return . Num . sum . (fromNum a :) . map (negate . fromNum) $ rgs
eval (Exp (Idn "*":args)) = return . Num . product . map fromNum $ args
eval (Exp (Idn "/":a:rgs)) = return . Num . product . (fromNum a :) . map (inverse . fromNum) $ rgs
    where inverse n = 1/n

eval (Exp [Idn "even", x]) = return . Bool . even . floor . fromNum $ x
eval (Exp [Idn "odd", x]) = return . Bool . odd . floor . fromNum $ x

eval (Exp (Idn "map":f:args)) = fmap Vec . mapM (run . Exp . (f :)). transpose . map fromList $ args
    where fromList (Vec xs) = xs
          fromList (Str cs) = map (Str . (: "")) cs

eval (Exp [Idn "filter", f, xs]) = fmap Vec . filterM (fmap useBool . run . Exp . (f:) . (:[])) . fromList $ xs

eval (Exp [Idn "reduce", f, xs]) = foldM1 (\a a' -> run $ Exp [f, a, a']) . fromList $ xs
    where foldM1 f (x:xs) = foldM f x xs

eval val = return val


interpret :: [Char] -> (String, ExitCode)
interpret input = case buildAST . reverse . trim . reverse . trim $ input of
        Right parsed      -> case recursiveExec Map.empty parsed of
            Right out     -> (show out, ExitSuccess)
            Left runError -> (show runError, ExitFailure 1)
        Left parseError   -> (show parseError, ExitFailure 1)
    where trim = dropWhile (`elem` " ~\n\r+\t")