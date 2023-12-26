module Types where

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
    deriving (Read, Eq)

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