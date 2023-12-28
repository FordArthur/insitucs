{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import Data.List
import Data.Data
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

type IType = String
type InTypes = [[IType]]

data RunError = TypeError ([IType], IType) | OutOfScope InsiType
    deriving (Eq, Show)

constrInString :: Data a => a -> String
constrInString = showConstr . toConstr

anyType :: [String]
anyType = ["Str", "Dict", "Num", "Vec", "Bool", "Null", "Idn", "Exp", "Clo"]

opers :: [(String, InTypes)]
opers = [("+", repeat ["Num"]), ("if", [anyType, anyType, anyType])]

typeCheck :: InTypes -> [InsiType] -> Either RunError [InsiType]
typeCheck checkers checking = case find (\(t, v) -> constrInString v `notElem` t) $ zip checkers checking of
        Just e  -> Left . TypeError . second show $ e
        Nothing -> Right checking