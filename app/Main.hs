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
                Clo ([InsiType], [InsiType]) | Opr String | Bol Bool
    deriving (Show, Read, Eq)

ignorable :: Parsec String [(InsiType, InsiType)] [Char]
ignorable = many1 (oneOf " ,\n")

accepted :: Parsec String [(InsiType, InsiType)] [Char]
accepted = many1 (noneOf " ,()[]{}#@")

derefer :: [(InsiType, InsiType)] -> InsiType -> InsiType
derefer binds (Idn var) = derefer binds val 
    where Just val = lookup (Idn var) binds
derefer _ val = val

bool :: Parsec String [(InsiType, InsiType)] InsiType
bool = toBool <$> (string "true" <|> string "false")
    where toBool "true"  = Bol True
          toBool "false" = Bol False

oper :: Parsec String [(InsiType, InsiType)] InsiType
oper = Opr <$> (string "+" <|> string "if")

iden :: Parsec String [(InsiType, InsiType)] InsiType
iden = getState >>= (\binds -> derefer binds . Idn <$> accepted)

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
insitux = choice [num, str, oper, bool, iden, vec, expr, dict, clo]

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
expr = eval

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
    skipMany ignorable
    args <- insitux `sepBy` many ignorable
    char ')'
    binds <- getState
    return $ apply func binds args

substitute :: [(InsiType, InsiType)] -> [InsiType] -> [InsiType]
substitute _ [] = []
substitute argLookUp (e:xps) = derefer argLookUp e : substitute argLookUp xps

apply :: InsiType -> [(InsiType, InsiType)] -> [InsiType] -> InsiType
apply (Num n) _ [Vec xs] = xs !! floor n
apply (Num n) _ [Str cs] = Str $ (cs !! floor n) : ""
apply (Vec xs) _ [thing] = if thing `elem` xs then thing else Str "lol!!!!"
apply (Dict ds) _ [key] = value
    where Just value = lookup key ds
apply (Clo (cloArgs, lam)) binds args = apply func binds subArgs
    where (func:subArgs) = substitute (zip cloArgs args) lam

apply (Opr "+") _ args = Num . sum $ map fromNum args
    where fromNum (Num n) = n
apply (Opr "if") _ (Bol p:a:b:_)
    | p         = a
    | otherwise = b

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