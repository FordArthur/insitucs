module Main where

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
      Stream, skipMany1, satisfy, choice )
data InsiType = Str String                   | Num Double | Vec [InsiType] | 
                Dict [(InsiType, InsiType)]  | Idn String | Exp [InsiType] |
                Clo ([InsiType], [InsiType]) | Bol Bool   | Null
    deriving (Show, Read, Eq)

toValueOrNull (Just x) = x
toValueOrNull Nothing = Null

ignorable :: Parsec String [(InsiType, InsiType)] [Char]
ignorable = many1 (oneOf " ,\n")

accepted :: Parsec String [(InsiType, InsiType)] [Char]
accepted = many1 (noneOf " ,()[]{}#@")

derefer :: [(InsiType, InsiType)] -> InsiType -> InsiType
derefer binds (Idn var)
    | var `elem` opers = Idn var
    | otherwise = derefer binds val
    where Just val = lookup (Idn var) binds
derefer _ val = val

bool :: Parsec String [(InsiType, InsiType)] InsiType
bool = toBool <$> (string "true" <|> string "false")
    where toBool "true"  = Bol True
          toBool "false" = Bol False

iden :: Parsec String [(InsiType, InsiType)] InsiType
iden = Idn <$> accepted

numP :: Parsec String [(InsiType, InsiType)] Double
numP = try (do
  int <- many1 digit
  char '.'
  dec <- many1 digit
  return $ read (int ++ "." ++ dec))
  <|> read <$> many1 digit
  <|> (char 'E' >> return (exp 1))
  <|> (string "PI" >> return pi)

num :: Parsec String [(InsiType, InsiType)] InsiType
num = (numP >>= toNum) <|> (char '-' >> numP >>= toNum . negate)
    where toNum i = return $ Num i

str :: Parsec String [(InsiType, InsiType)] InsiType
str = between (char '\"') (char '\"') (Str <$> many (noneOf "\""))

insitux :: Parsec String [(InsiType, InsiType)] InsiType
insitux = choice [num, str, bool, iden, vec, expr, dict, clo]

vec :: Parsec String [(InsiType, InsiType)] InsiType
vec = between (char '[') (char ']') (Vec <$> insitux `sepBy` ignorable)

pair :: Parsec String [(InsiType, InsiType)] (InsiType, InsiType)
pair = do
    key <- insitux
    skipMany (oneOf " ,")
    value <- insitux
    return (key, value)

dict :: Parsec String [(InsiType, InsiType)] InsiType
dict = between (char '{') (char '}') (Dict <$> pair `sepBy` ignorable)

expr :: Parsec String [(InsiType, InsiType)] InsiType
expr = between (char '(' >> skipMany ignorable) (skipMany ignorable >> char ')') $ try bind <|> eval 

bind :: Parsec String [(InsiType, InsiType)] InsiType -- will get declarified
bind = do
    string "let"
    skipMany ignorable
    binds <- concat <$> bindings `sepBy` ignorable
    modifyState $ (++) binds
    let (_, value) = last binds
        in return value

bindings :: Parsec String [(InsiType, InsiType)] [(InsiType, InsiType)]
bindings = do
        name <- iden
        skipMany ignorable
        binding <- insitux
        return [(name, binding)]
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
            where together (Vec n) (Vec b) = zip n b
                  together (Vec n) (Str b) = zipWith (\n' b' -> (n', Str $ b' : "")) n b

eval :: Parsec String [(InsiType, InsiType)] InsiType
eval = do
    exprs <- insitux `sepBy` many ignorable
    binds <- getState
    return $ apply . map (derefer binds) $ exprs

substitute :: [(InsiType, InsiType)] -> [InsiType] -> [InsiType]
substitute _ [] = []
substitute argLookUp (e:xps) = derefer argLookUp e : substitute argLookUp xps

opers :: [String]
opers = ["+", "if"]

apply :: [InsiType] -> InsiType
apply [Num n, Vec xs] = xs !! floor n
apply [Num n, Str cs] = Str $ cs !! floor n : ""
apply [Vec xs, thing] = if thing `elem` xs then thing else Null
apply [Dict ds, key] = value
    where value = toValueOrNull $ lookup key ds
apply (Clo (cloArgs, lam):args) = apply exprs
    where exprs = substitute (zip cloArgs args) lam

apply (Idn "+":args) = Num . sum $ map fromNum args
    where fromNum (Num n) = n
apply (Idn "if":Bol p:a:b:_)
    | p         = a
    | otherwise = b

getArgs :: [InsiType] -> [InsiType] -> [InsiType] -> ([InsiType], [InsiType])
getArgs args things [] = (reverse args, reverse things)
getArgs args things (Idn ('%':n:_):xs) = getArgs (Idn (n:""):args) (Idn (n:""):things) xs
getArgs args things (x:xs) = getArgs args (x:things) xs

clo :: Parsec String [(InsiType, InsiType)] InsiType
clo = between (string "#(") (char ')') (Clo . getArgs [] [] <$> (insitux `sepBy` ignorable))

-- testing function
withState :: Parsec s u a -> Parsec s u (a, u)
withState p = do
    a <- p
    s <- getState
    return (a, s)

-- "repl" mode
main :: IO ()
main = do
    i <- getLine
    print $ runParser (withState $ many insitux `sepBy` ignorable) [] "" i
    main