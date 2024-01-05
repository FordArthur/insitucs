{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import Data.List
import Data.Data
import Data.Functor
import Control.Arrow

type Label = (Maybe String, Maybe (Int, Int))

succArgs :: Enum c => (a, Maybe (b, c)) -> (a, Maybe (b, c))
succArgs = second (second succ <$>)

succNest :: Enum b => (a, Maybe (b, c)) -> (a, Maybe (b, c))
succNest = second (first succ <$>)

data InsiType = Str String                          | Num Double  | Vec [InsiType] | 
                Dict [(InsiType, InsiType)]         | Idn Label String  | Exp [InsiType] |
                Clo String Label ([InsiType], [InsiType]) | Bool Bool   | Null           |
                Binds [(InsiType, InsiType)]
    deriving (Read, Eq, Data)

type Defs = [(InsiType, InsiType)]

showSpace :: Show a => a -> String
showSpace x = ' ' : show x


instance Show InsiType where 
    show (Str s) = '\"' : s ++ "\""
    show (Dict d) = '{' : concatMap (\(k, v) -> ", " ++ show k ++ " " ++ show v) d ++ "}"
    show (Num n)
        | n == fromInteger (round n) = show $ round n
        | otherwise = show n
    show (Vec v) = '[' : concatMap showSpace v ++ " ]"
    show (Bool True) = "true"
    show (Bool False) = "false"
    show Null = "null"
    show (Idn _ i) = i
    show (Exp (e:es)) = '(' : show e ++ concatMap showSpace es ++ ")"
    show (Binds bs) = let (_, v) = last bs in show v
    show (Clo "lam" _ (_, l:ls)) = "#(" ++ show l ++ concatMap showSpace ls ++ ")"
    show (Clo "part" _ (_, p:ps)) = "@(" ++ show p ++ concatMap showSpace ps ++ ")"
    show (Clo "fun" _ (a:as, fs)) = "(fn" ++ showSpace a ++ concatMap showSpace as ++ concatMap showSpace fs ++ ")"

toValueOrNullT :: (t -> InsiType) -> Maybe t -> InsiType
toValueOrNullT t (Just x) = t x
toValueOrNullT _ Nothing = Null

type ArityCheck = (Ordering, Int)
type IType = String
type InTypes = [([IType], IsLazy)]
type IsLazy = Bool

data RunError = TypeError ([IType], IType) | OutOfScope InsiType | ArityError ArityCheck Int
    deriving (Eq, Show)

constrInString :: Data a => a -> String
constrInString = showConstr . toConstr

anyType :: [IType]
anyType = ["Str", "Dict", "Num", "Vec", "Bool", "Null", "Idn", "Exp", "Clo"]

anyTypeLazy :: ([IType], IsLazy)
anyTypeLazy = (anyType,False)

opers :: [(String, (InTypes, ArityCheck))]
opers = [("+", (repeat (["Num"], False), (GT, 1))), ("if", ([(anyType, False), (anyType, True), (anyType, True)], (EQ, 2))), ("=", (repeat anyTypeLazy, (GT, 1)))]

builtIn :: [(String, (InTypes, ArityCheck))]
builtIn = [("Num", ([(["Str", "Vec"], False)], (EQ, 1))), ("Vec", ([anyTypeLazy], (EQ, 1))), ("Dict", ([anyTypeLazy], (EQ, 1))), ("Bool", ([anyTypeLazy, anyTypeLazy], (EQ, 2))), ("Clo", (repeat anyTypeLazy, (GT, -1))), ("Exp", (repeat anyTypeLazy, (GT, -1)))]

typeCheck :: InsiType -> (InTypes, ArityCheck) -> [InsiType] -> Either RunError InsiType
typeCheck f (checkers, arityPredicate) checking = (zipIfPred arityPredicate checkers checking >>= findTypeMismatch) <&> constr . (f:)
    where findTypeMismatch tvs = 
            case find (\((t, _), v) -> constrInString v `notElem` t) tvs of
                    Just e  -> Left . TypeError . (fst *** show) $ e
                    Nothing -> Right checking
          zipIfPred p@(o, _) as bs
            | o /= EQ || runArityCheck p bl = Right $ zip as bs
            | otherwise                     = Left  $ ArityError p bl
            where bl = length bs
          runArityCheck (shouldBe, n) n' = shouldBe == n' `compare` n
          constr = if runArityCheck arityPredicate (length checking) then Exp else if fst arityPredicate /= EQ then (\exp -> Clo "part" (Nothing, Nothing) ([],exp)) else Exp