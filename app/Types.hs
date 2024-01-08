{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import Data.Char ( isDigit )
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
    show (Vec (v:vs)) = '[' : show v ++ concatMap showSpace vs ++ "]"
    show (Bool True) = "true"
    show (Bool False) = "false"
    show Null = "null"
    show (Idn _ i) = if all isDigit i then '%':i else i
    show (Exp (e:es)) = '(' : show e ++ concatMap showSpace es ++ ")"
    show (Binds bs) = let (_, v) = last bs in show v
    show (Clo "lam" _ (_, l:ls)) = "#(" ++ show l ++ concatMap showSpace ls ++ ")"
    show (Clo "part" _ (_, p:ps)) = "@(" ++ show p ++ concatMap showSpace ps ++ ")"
    show (Clo "fun" _ (as, fs)) = "(fn" ++ concatMap showSpace as ++ concatMap showSpace fs ++ ")"

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

data RunError = TypeError ([IType], IType) | OutOfScope InsiType | ArityError ArityCheck Int
    deriving (Eq, Show)

constrInString :: Data a => a -> String
constrInString = showConstr . toConstr

anyType :: [IType]
anyType = ["Str", "Dict", "Num", "Vec", "Bool", "Null", "Idn", "Exp", "Clo"]

anyTypeStrict :: ([IType], IsLazy)
anyTypeStrict = (anyType,False)

opers :: [(String, (InTypes, ArityCheck))]
opers = [("+", numOper),
         ("-", numOper),
         ("*", numOper),
         ("/", numOper),
         ("even", ([(["Num"], False)], (EQ, 1))),
         ("odd", ([(["Num"], False)], (EQ, 1))),
         ("if", ([(anyType, False), (anyType, True), (anyType, True)], (EQ, 3))), 
         ("=", strictNAry),
         ("and", strictNAry),
         ("or", strictNAry),
         ("map", ((anyType, False):repeat (["Vec", "Str"], False), (GT, 1))),
         ("filter", (replicate 2 (anyType, False), (GT, 1))),
         ("reduce", (replicate 2 (anyType, False), (GT, 1)))
        ]
    where numOper = (repeat (["Num"], False), (GT, 1))
          strictNAry = (repeat anyTypeStrict, (GT, 1))

builtIn :: [(String, (InTypes, ArityCheck))]
builtIn = [("Num", ([(["Str", "Vec"], False)], (EQ, 1))), ("Vec", ([anyTypeStrict], (EQ, 1))), ("Dict", ([anyTypeStrict], (EQ, 1))), ("Bool", ([anyTypeStrict, anyTypeStrict], (EQ, 2))), ("Clo", (repeat anyTypeStrict, (GT, -1))), ("Exp", (repeat anyTypeStrict, (GT, -1)))]

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