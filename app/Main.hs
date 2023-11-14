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
      Stream, skipMany1 )
data InsiType = Str String                  | Num Double | Vec [InsiType] | 
                Dict [(InsiType, InsiType)] | Idn String | Exp [InsiType] |
                Clo ([InsiType], [InsiType])
    deriving (Show, Read, Eq)

ignorable :: Parsec String [(InsiType, InsiType)] [Char]
ignorable = many1 (oneOf " ,\n")

accepted :: Parsec String [(InsiType, InsiType)] [Char]
accepted = many1 (noneOf " ,()[]{}#@")

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
num = (numP >>= toNum) <|> do 
    char '-'
    numP >>= toNum . negate
    where toNum i = return $ Num i

str :: Parsec String [(InsiType, InsiType)] InsiType
str = between (char '\"') (char '\"') (Str <$> many (noneOf "\""))

insitux :: Parsec String [(InsiType, InsiType)] InsiType
insitux = num <|> str <|> iden <|> vec <|> expr <|> dict <|> clo

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
expr = bind <|> eval

bind :: Parsec String [(InsiType, InsiType)] InsiType -- will get declarified
bind = do
    char '('
    skipMany ignorable
    string "let"
    skipMany ignorable
    name <- iden
    skipMany ignorable
    val <- insitux
    char ')'
    modifyState $ (:) (name, val)
    return val

eval :: Parsec String [(InsiType, InsiType)] InsiType
eval = do
    char '('
    skipMany ignorable
    func <- insitux
    args <- insitux `sepBy` many ignorable
    char ')'
    binds <- getState
    return $ apply func binds args

apply :: InsiType -> [(InsiType, InsiType)] -> [InsiType] -> InsiType
apply (Num n) _ [Vec xs] = xs !! floor n
apply (Num n) _ [Str cs] = Str $ (cs !! floor n) : ""
apply (Vec xs) _ [thing] = if thing `elem` xs then thing else Str "lol!!!!"
apply (Dict ds) _ [key] = value
    where Just value = lookup key ds
apply (Clo (cloArgs, lam)) binds args = 
apply i binds args = apply func binds args
    where (Just func) = lookup i binds

getArgs :: [InsiType] -> [InsiType] -> [InsiType] -> ([InsiType], [InsiType])
getArgs args things [] = (args, things)
getArgs args things (Idn ('%':n:_):xs) = getArgs (Idn (n:""):args) things xs
getArgs args things (x:xs) = getArgs args (x:things) xs

clo :: Parsec String [(InsiType, InsiType)] InsiType -- Makes it :)))
clo = between (string "#(") (char ')') (Clo . getArgs [] [] <$> (insitux `sepBy` ignorable))

main :: IO ()
main = do
    ixFile <- getLine
    file <- readFile ixFile
    print (runParser insitux [] ixFile file)