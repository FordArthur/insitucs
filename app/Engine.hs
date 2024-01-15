module Engine where

import System.Exit ( ExitCode(..) )
import GHC.IO (unsafePerformIO)

import Data.Char (isDigit)
import Data.List (isPrefixOf, elemIndex, transpose)
import qualified Data.Map as Map
import Data.Functor (($>))
import Control.Monad.ST (runST)
import Data.STRef ( modifySTRef', newSTRef, readSTRef )
import qualified Control.Concurrent.STM as STM
import Control.Monad (filterM, foldM, forM_, replicateM_)
import Control.Monad.State.Strict
    ( 
      StateT (runStateT),
      MonadTrans(lift),
      MonadState(get),
      state,
      mplus,
      modify,
      evalStateT,
      execStateT,
      (>=>)
    )
import Parser (buildAST)
import Types (RunError(..), Defs, InsiType(..), IsLazy, useBool, fromNum, fromList, toValueOrNullT, opers, builtIn, typeCheck, constrInString)
import Control.Concurrent (forkIO)

derefer :: InsiType -> StateT Defs (Either RunError) InsiType
derefer i@(Idn _ var)
    | var `Map.member` opers = return i
    | otherwise = get >>= lift . unbind >>= derefer
    where unbind binds = throwOutOfScope $ Map.lookup (Idn True var) binds `mplus` Map.lookup (Idn False var) binds
            where throwOutOfScope (Just x) = Right x
                  throwOutOfScope Nothing  = Left $ OutOfScope i
derefer (Vec v) = Vec <$> substitute v
derefer (Exp e) = Exp <$> substitute e
derefer (Clo l f) = return $ Clo l f
derefer b@(Binds{}) = eval b
derefer val = return val

check :: InsiType -> Either RunError InsiType
check (Exp (h@(Idn _ f'):args')) = typeCheck h  (todoLookup f' opers) args'
check (Exp (f':args'))           = typeCheck f' (todoLookup (constrInString f') builtIn) args'
check ix                         = Right ix

simplify :: InsiType -> [IsLazy] -> StateT Defs (Either RunError) InsiType
simplify (Exp (f:args)) lazy = Exp <$> (mapM (\(ix, lazy) -> if lazy then return ix else run ix) . ((f, False) :) . zip args) lazy
simplify ix        _    = return ix

run :: InsiType -> StateT Defs (Either RunError) InsiType
run f = mkLazy f >>= uncurry simplify >>= (lift . check) >>= eval
    where mkLazy exp@(Exp (Idn _ f:_)) = return (exp, map snd . fst . todoLookup f $ opers)
          mkLazy exp@(Exp (built:_))   = return (exp, map snd . fst . todoLookup (constrInString built) $ builtIn)
          mkLazy ix                    = return (ix , [])

substitute :: [InsiType] -> StateT Defs (Either RunError) [InsiType]
substitute = mapMST derefer
    where mapMST f xs = 
            runST $ do 
            ys' <- newSTRef $ return []
            forM_ xs (\x -> modifySTRef' ys' (\ys -> (++) <$> ys <*> fmap (: []) (f x)))
            readSTRef ys'

recursiveExec :: [InsiType] -> StateT Defs (Either RunError) InsiType
recursiveExec (f:fs) = foldlST (\s f -> s >> derefer f >>= run) (derefer f >>= run) fs 
                       >>= (modify (Map.filterWithKey (\(Idn scope _) _ -> not scope )) $> )
    where foldlST f acc xs = 
            runST $ do
            acc' <- newSTRef acc
            forM_ xs (\x -> modifySTRef' acc' (`f` x))
            readSTRef acc'

todoLookup :: String -> Map.Map String v -> v
todoLookup x xs = 
    case Map.lookup x xs of
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

eval (Exp (Clo "lam" (cloArgs, lam):args)) =
    modify (Map.fromList ((Idn True "args", Vec args) : zip cloArgs (args ++ repeat Null)) `Map.union`) 
    >> return (Exp lam) 
    >>= derefer 
    >>= eval

eval (Exp (Clo "part" (cloArgs, lam):args)) = 
    modify (Map.fromList ((Idn True "args", Vec args) : zip cloArgs (args ++ repeat Null)) `Map.union`) 
    >> return (Exp $ lam ++ args ) 
    >>= derefer 
    >>= eval
eval (Exp (Clo "fun" (cloArgs, func):args)) = modify (Map.fromList ((Idn True "args", Vec args) : zip cloArgs (args ++ repeat Null)) `Map.union`) >> recursiveExec func

eval (Exp [Idn _ "if", p, a])
    | useBool p && (p /= Null) = run a
    | otherwise                = return Null

eval (Exp (Idn _ "if": p: a: b:_))
    | useBool p && (p /= Null) = run a
    | otherwise                = run b

eval (Exp (Idn _ "=":a:rgs)) = return . Bool $ all (a ==) rgs

eval (Exp (Idn _ "and":args)) = return . Bool . all useBool $ args
eval (Exp (Idn _ "or":args))  = return . Bool . any useBool $ args

eval (Exp (Idn _ "+":args)) = return . Num . sum . map fromNum $ args
eval (Exp (Idn _ "-":a:rgs)) = return . Num . sum . (fromNum a :) . map (negate . fromNum) $ rgs
eval (Exp (Idn _ "*":args)) = return . Num . product . map fromNum $ args
eval (Exp (Idn _ "/":a:rgs)) = return . Num . product . (fromNum a :) . map (inverse . fromNum) $ rgs
    where inverse n = 1/n

eval (Exp [Idn _ "even", x]) = return . Bool . even . floor . fromNum $ x
eval (Exp [Idn _ "odd", x]) = return . Bool . odd . floor . fromNum $ x

eval (Exp (Idn _ "map":f:args)) = fmap Vec . mapM (run . Exp . (f :)). transpose . map fromList $ args
    where fromList (Vec xs) = xs
          fromList (Str cs) = map (Str . (: "")) cs

eval (Exp [Idn _ "filter", f, xs]) = fmap Vec . filterM (fmap useBool . run . Exp . (f:) . (:[])) . fromList $ xs

eval (Exp [Idn _ "reduce", f, xs]) = foldM1 (\a a' -> run $ Exp [f, a, a']) . fromList $ xs
    where foldM1 f (x:xs) = foldM f x xs

eval (Exp (Idn _ "print": xs)) = unsafePerformIO . (\toPrint -> putStrLn toPrint >> return (return Null)) . unwords . map show $ xs

eval val = return val


interpret :: String -> IO (String, ExitCode)
-- interpret input = do
--    toParse' <- STM.newTVarIO $ Right []
--    forkIO . STM.atomically . STM.writeTVar toParse' $ buildAST input -- TODO: concurrently parse input
--    parsed <- STM.readTVarIO toParse'
{-
    case buildAST input of
        Right toRun -> 
            do
            toRun' <- STM.newTVarIO . state $ \ _ -> ([], Map.empty)
            forM_ toRun $ \ atom -> do
                forkIO 
                . STM.atomically 
                . STM.modifyTVar toRun' 
                $ \ x -> (:) <$> (derefer atom >>= run) <*> x
            runned <- STM.readTVarIO toRun'
            case runStateT runned Map.empty of
                Right out     -> return (show out       , ExitSuccess)
                Left runError -> return (show runError  , ExitFailure 1)
        Left parseError       -> return (show parseError, ExitFailure 1)
-}

interpret input = case buildAST input of
        Right parsed      -> case evalStateT (recursiveExec parsed) Map.empty of
            Right out     -> return (show out, ExitSuccess)
            Left runError -> return (show runError, ExitFailure 1)
        Left parseError   -> return (show parseError, ExitFailure 1)
