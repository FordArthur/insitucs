module Main where

import Data.List (isPrefixOf)
import Control.Monad.State (evalState, execState, runState, State, state, get, put, modify)

import Text.Parsec
    ( char,
      digit,
      noneOf,
      oneOf,
      between,
      many1,
      sepBy,
      (<|>),
      many,
      parse,
      runParser,
      skipMany,
      try,
      string,
      getState,
      putState,
      modifyState,
      Parsec,
      Stream, skipMany1, satisfy, choice, endBy )

data InsiType = Str String                          | Num Double  | Vec [InsiType] | 
                Dict [(InsiType, InsiType)]         | Idn String  | Exp [InsiType] |
                Clo String ([InsiType], [InsiType]) | Bool Bool   | Null           |
                Binds [(InsiType, InsiType)]
    deriving (Read, Eq)

type Defs = [(InsiType, InsiType)]

showSpace :: Show a => a -> [Char]
showSpace x = ' ' : show x

instance Show  InsiType where 
    show (Str s) = '\"' : s ++ "\""
    show (Dict d) = '{' : concatMap (\(k, v) -> ", " ++ show k ++ " " ++ show v) d ++ "}"
    show (Num n)
        | n == fromInteger (round n) = show $ round n
        | otherwise = show n
    show (Vec v) = '[' : concatMap showSpace v ++ " ]"
    show (Bool True) = "true"
    show (Bool False) = "false"
    show Null = "null"
    show (Idn i) = i
    show (Exp (e:es)) = '(' : show e ++ concatMap showSpace es ++ ")"
    show (Binds bs) = let (_, v) = last bs in show v
    show (Clo "lam" (_, l:ls)) = "#(" ++ show l ++ concatMap showSpace ls ++ ")"
    show (Clo "part" (_, p:ps)) = "@(" ++ show p ++ concatMap showSpace ps ++ ")"
    show (Clo "fun" (a:as, fs)) = "(fn " ++ show a ++ concatMap showSpace as ++ concatMap showSpace fs ++ ")"

toValueOrNull :: Maybe InsiType -> InsiType
toValueOrNull (Just x) = x
toValueOrNull Nothing = Null

ignorable :: Parsec String () [Char]
ignorable = many1 (oneOf " ,\n")

accepted :: Parsec String () [Char]
accepted = many1 (noneOf " ,()[]{}#@;")

derefer :: [(InsiType, InsiType)] -> InsiType -> InsiType
derefer binds (Idn var)
    | var `elem` opers = Idn var
    | otherwise = derefer binds val
    where Just val = lookUpOneOf [Idn var, Idn $ '\"' : var ++ "\""] binds
            where lookUpOneOf _ [] = Nothing
                  lookUpOneOf xs ((k, v):ds)
                      | k `elem` xs = Just v
                      | otherwise   = lookUpOneOf xs ds
derefer binds (Vec v) = Vec . substitute binds $ v
derefer binds (Exp e) = evalState (apply $ Exp e) binds
derefer binds (Clo t (args, lams)) = curry (Clo t) args . substitute binds $ lams
derefer _ val = val

substitute :: [(InsiType, InsiType)] -> [InsiType] -> [InsiType]
substitute argLookUp = map (derefer argLookUp)

bool :: Parsec String () InsiType
bool = toBool <$> (string "true" <|> string "false")
    where toBool "true"  = Bool True
          toBool "false" = Bool False

iden :: Parsec String () InsiType
iden = Idn <$> accepted

numP :: Parsec String () Double
numP = try (do
  int <- many1 digit
  char '.'
  dec <- many1 digit
  return $ read (int ++ "." ++ dec))
  <|> read <$> many1 digit
  <|> (char 'E' >> return (exp 1))
  <|> (string "PI" >> return pi)

num :: Parsec String () InsiType
num = (numP >>= toNum) <|> (char '-' >> numP >>= toNum . negate)
    where toNum i = return $ Num i

str :: Parsec String () InsiType
str = between (char '\"') (char '\"') (Str <$> many (noneOf "\""))

trying :: [Parsec s u a] -> Parsec s u a
trying [p] = p
trying (p:ps) = try p <|> trying ps

insitux :: Parsec String () InsiType
insitux = trying [clo, num, str, bool, iden, vec, expr, dict]

vec :: Parsec String () InsiType
vec = between (char '[') (char ']') (Vec <$> insitux `sepBy` ignorable)

pair :: Parsec String () (InsiType, InsiType)
pair = do
    key <- insitux
    skipMany (oneOf " ,")
    value <- insitux
    return (key, value)

dict :: Parsec String () InsiType
dict = between (char '{') (char '}') (Dict <$> pair `sepBy` ignorable)

expr :: Parsec String () InsiType
expr = between (char '(' >> skipMany ignorable) (skipMany ignorable >> char ')') $ try bind <|> eval 

bind :: Parsec String () InsiType -- will get declarified
bind = do
    isLocal <- string "let" <|> string "var"
    skipMany ignorable
    binds <- concat <$> bindings (isLocal == "let") `sepBy` ignorable
    return $ Binds binds

bindings :: Bool -> Parsec String () [(InsiType, InsiType)]
bindings local = do
        name <- iden
        skipMany ignorable
        binding <- insitux
        return [(scopeIf local name, binding)]
    <|> do
        pvName <- vec
        skipMany ignorable
        pvBinding <- vec <|> str
        return $ together pvName pvBinding
    <|> do
        Dict pdName <- dict
        skipMany ignorable
        Dict pdBinding <- dict
        return $ map (\(k, v) -> (v, toValueOrNull $ lookup k pdBinding)) pdName
            where together (Vec n) (Vec b) = zipWith (\n' b' -> (scopeIf local n', b')) n b
                  together (Vec n) (Str b) = zipWith (\n' b' -> (scopeIf local n', Str $ b' : "")) n b
                  scopeIf True (Idn name) = Idn $ '\"' : name ++ "\""
                  scopeIf _    name = name 
                  
eval :: Parsec String () InsiType
eval = do
    exprs <- insitux `sepBy` many ignorable
    binds <- getState
    return $ Exp exprs -- $ apply . map (derefer binds) $ exprs

opers :: [String]
opers = ["+", "if"]

(>>>==) :: Defs -> [InsiType] -> State Defs InsiType
s' >>>== [f] = state $ \s -> runState (apply f) (s ++ s')
s >>>== (f:fs) = (s' ++ s) >>>== fs
     where s' = execState (apply f) s

apply ::  InsiType -> State Defs InsiType

apply (Binds b) = modify (++ b) >> return a
    where (_, a) = last b

apply (Exp [Num n, Vec xs]) = return $ xs !! floor n
apply (Exp [Num n, Str cs]) = return . Str $ cs !! floor n : ""
apply (Exp [Num n, i]) = get >>= (\s -> apply . Exp $ [Num n, derefer s i])
apply (Exp [Vec xs, thing]) = return $ if thing `elem` xs then thing else Null

apply (Exp [Dict ds, key]) = return value
    where value = toValueOrNull $ lookup key ds
apply (Exp (Clo "lam" (cloArgs, lam):args)) = get >>= exprs >>= apply
     where exprs s = return . Exp $ substitute (zip cloArgs args ++ s) lam
apply (Exp (Clo "part" (cloArgs, lam):args)) = get >>= exprs >>= apply
    where exprs s = return . Exp . (++ args) $ substitute (zip cloArgs args ++ s) lam
apply (Exp (Clo "fun" (cloArgs, func):args)) = get >>= exprs
    where exprs s = (zip cloArgs args ++ s) >>>== func
          getExp (Exp e) = e

apply (Exp (Idn "+":args)) = get >>= (\s -> return . Num . sum $ map (fromNum . derefer s) args)
    where fromNum (Num n) = n -- temporal solution, typechecking will solve this

apply (Exp (Idn "if":Bool p:a:b:_))
    | p         = apply a
    | otherwise = apply b

apply (Exp (Idn i:args)) = get >>= (\s -> apply . Exp $ derefer s (Idn i) : args)
apply val = return val

getArgs :: [InsiType] -> [InsiType] -> [InsiType] -> ([InsiType], [InsiType])
getArgs args things [] = (reverse args, reverse things)
getArgs args things (Idn ('%':n:_):xs) = getArgs (Idn (n:""):args) (Idn (n:""):things) xs
getArgs args things (x:xs) = getArgs args (x:things) xs

clo :: Parsec String () InsiType
clo = between (string "#(" >> skipMany ignorable) (skipMany ignorable >> char ')') (Clo "lam" . getArgs [] [] <$> (insitux `sepBy` ignorable))
    <|> between (string "@(" >> skipMany ignorable) (skipMany ignorable >> char ')') (Clo "part" . getArgs [] [] <$> (insitux `sepBy` ignorable))
    <|> do
        string "(fn"
        skipMany ignorable
        cloArgs <- iden `endBy` ignorable
        funcs <- insitux `sepBy` ignorable
        char ')'
        return $ Clo "fun" (cloArgs, funcs)

recursiveFeed :: (Defs -> InsiType -> (InsiType, Defs)) -> Defs -> [InsiType] -> [(InsiType, Defs)]
recursiveFeed _ _ [] = []
recursiveFeed f s (x:xs) = (a', s') : recursiveFeed f (s ++ s') xs
    where (a', s') = f s x

-- "repl" mode
main :: IO ()
main = do
    i <- getLine
    putStrLn $ case parse (insitux `sepBy` ignorable) "" i of
        (Right xs) -> show $ recursiveFeed (\s a -> runState (apply a) s) [] xs
        (Left e)  -> show e
    main