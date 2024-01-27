{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import Data.Char ( isDigit )
import Data.List ( find )
import qualified Data.Map as Map
import Control.Monad.ST (runST)
import Data.STRef ( modifySTRef', newSTRef, readSTRef )
import Data.Data ( Data(toConstr), showConstr )
import Data.Functor ( (<&>) )
import Control.Arrow ( Arrow(..) )
import Control.Monad (forM_)

succArgs :: Enum c => (a, Maybe (b, c)) -> (a, Maybe (b, c))
succArgs = second (second succ <$>)

succNest :: Enum b => (a, Maybe (b, c)) -> (a, Maybe (b, c))
succNest = second (first succ <$>)

type IsScoped = Bool
data InsiType = Str String                          | Num Double  | Vec [InsiType] | 
                Dict (Map.Map InsiType InsiType)    | Idn IsScoped String  | Exp [InsiType] |
                Clo String ([InsiType], [InsiType]) | Bool Bool   | Null           |
                Binds IsScoped InsiType (Map.Map InsiType InsiType)
    deriving (Eq, Ord, Data)

type Defs = Map.Map InsiType InsiType

showSpace :: Show a => a -> String
showSpace x = ' ' : show x

instance Show InsiType where 
    show (Str s) = '\"' : s ++ "\""
    show (Dict d) = '{' : concatMap (\(k, v) -> ", " ++ show k ++ " " ++ show v) (Map.toList d) ++ "}"
    show (Num n)
        | n == fromInteger (round n) = show $ round n
        | otherwise = show n
    show (Vec (v:vs)) = '[' : show v ++ concatMap showSpace vs ++ "]"
    show (Bool True) = "true"
    show (Bool False) = "false"
    show Null = "null"
    show (Idn _ i) = if all isDigit i then '%':i else i
    show (Exp (e:es)) = '(' : show e ++ concatMap showSpace es ++ ")"
    show (Binds scope _ bs) = '(' : if scope then "let " else "var " ++ concatMap (\(k, v) -> ", " ++ show k ++ " " ++ show v) (Map.toList bs) ++ ")"
    show (Clo "lam" (_, l:ls)) = "#(" ++ show l ++ concatMap showSpace ls ++ ")"
    show (Clo "part" (_, p:ps)) = "@(" ++ show p ++ concatMap showSpace ps ++ ")"
    show (Clo "fun" (as, fs)) = "(fn" ++ concatMap showSpace as  ++ "," ++ concatMap showSpace fs ++ ")"

toValueOrNullT :: (t -> InsiType) -> Maybe t -> InsiType
toValueOrNullT t (Just x) = t x
toValueOrNullT _ Nothing = Null

useBool :: InsiType -> Bool
useBool (Bool p) = p
useBool _        = True

fromList :: InsiType -> [InsiType]
fromList (Vec xs) = xs
fromList (Str cs) = map (Str . (: "")) cs

fromNum :: InsiType -> Double
fromNum (Num n) = n 

type ArityCheck = (Ordering, Int)
type IType = String
type InTypes = [([IType], IsLazy)]
type IsLazy = Bool

data RunError = TypeError [IType] IType | OutOfScope InsiType | ArityError ArityCheck Int
    deriving (Eq, Show)

constrInString :: Data a => a -> String
constrInString = showConstr . toConstr

anyType :: [IType]
anyType = ["Str", "Dict", "Num", "Vec", "Bool", "Null", "Idn", "Exp", "Clo"]

anyTypeStrict :: ([IType], IsLazy)
anyTypeStrict = (anyType,False)

opers :: Map.Map String (InTypes, ArityCheck)
opers = Map.fromList [("+", numOper),
         ("-", numOper),
         ("*", numOper),
         ("/", numOper),
         ("even", ([(["Num"], False)], (EQ, 1))),
         ("odd", ([(["Num"], False)], (EQ, 1))),
         ("if", ([(anyType, False), (anyType, True), (anyType, True)], (GT, 1))), 
         ("=", strictNAry),
         ("and", strictNAry),
         ("or", strictNAry),
         ("map", ((anyType, False):repeat (["Vec", "Str"], False), (GT, 1))),
         ("filter", (replicate 2 (anyType, False), (GT, 1))),
         ("reduce", (replicate 2 (anyType, False), (GT, 1))),
         ("print", (repeat (anyType, False), (GT, 0)))
        ]
    where numOper = (repeat (["Num"], False), (GT, 1))
          strictNAry = (repeat anyTypeStrict, (GT, 1))

builtIn :: Map.Map String (InTypes, ArityCheck)
builtIn = Map.fromList [("Num", ([(["Str", "Vec"], False)], (EQ, 1))), ("Vec", ([anyTypeStrict], (EQ, 1))), ("Dict", ([anyTypeStrict], (EQ, 1))), ("Bool", ([anyTypeStrict, anyTypeStrict], (EQ, 2))), ("Clo", (repeat anyTypeStrict, (GT, -1))), ("Exp", (repeat anyTypeStrict, (GT, -1)))]

typeCheck :: InsiType -> (InTypes, ArityCheck) -> [InsiType] -> Either RunError InsiType
typeCheck f (checkers, arityPredicate@(order, _)) checking
    | order /= EQ || runArityCheck arityPredicate (length checking) = constr  . (f:) <$> runTypeChecks checkers checking
    where runTypeChecks _ [] = Right []
          runTypeChecks shoulds ares
            = runST $ do
                are' <- newSTRef $ Right []
                shoulds' <- newSTRef shoulds
                forM_ (reverse ares) $ \ is' -> let is = constrInString is' in do
                    should <- head <$> readSTRef shoulds'
                    modifySTRef' are' ((:) <$> runTypeCheck should is is' <*>)
                readSTRef are'
                where runTypeCheck (s, _) i i'
                        | i `elem` s  = Right i'
                        | otherwise   = Left $ TypeError s i
          runArityCheck (shouldBe, n) n' = shouldBe == n' `compare` n
          constr = if not (runArityCheck arityPredicate (length checking)) && fst arityPredicate /= EQ
                then (\exp -> Clo "part" ([], exp))
                else Exp 