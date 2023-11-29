module Parser where

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
      ParseError,
      Stream, skipMany1, satisfy, choice, endBy )

import Types
    ( InsiType(Exp, Bool, Num, Binds, Clo, Vec, Str, Idn, Dict), Label,
      toValueOrNullT, labellessClo, labelWith)

type Pos = (Maybe String, (Int, Int))

succArgs :: Enum c => (a, (b, c)) -> (a, (b, c))
succArgs (a, (b, c)) = (a, (b, succ c))

succNest :: Enum b => (a, (b, c)) -> (a, (b, c))
succNest (a, (b, c)) = (a, (succ b, c))


enumSepBy1 :: a -> (a -> b) -> (b -> Parsec s u c) -> Parsec s u sep -> Parsec s u [c]
enumSepBy1 a succ' p sep = do
  x <- p'
  xs <- many (sep >> p')
  return (x:xs)
    where p' = p $ succ' a

ignorable :: Parsec String () [Char]
ignorable = many1 (oneOf " ,\n")

accepted :: Parsec String () [Char]
accepted = many1 (noneOf " ,()[]{}#@;")

getArgs :: [InsiType] -> [InsiType] -> [InsiType] -> ([InsiType], [InsiType])
getArgs args things [] = (reverse args, reverse things)
getArgs args things (Idn ('%':n:_):xs) = getArgs (Idn (n:""):args) (Idn (n:""):things) xs
getArgs args things (x:xs) = getArgs args (x:things) xs

clo :: Pos -> Parsec String () InsiType
clo label = between (string "#(" >> skipMany ignorable) (skipMany ignorable >> char ')') (labellessClo "lam" . getArgs [] [] <$> (insitux label `sepBy` ignorable))
    <|> between (string "@(" >> skipMany ignorable) (skipMany ignorable >> char ')') (labellessClo "part" . getArgs [] [] <$> (insitux label `sepBy` ignorable))
    <|> do
        string "(fn"
        skipMany ignorable
        cloArgs <- iden `endBy` ignorable
        funcs <- insitux label `sepBy` ignorable
        char ')'
        return $ labellessClo "fun" (cloArgs, funcs)

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

insitux :: Pos -> Parsec String () InsiType
insitux p = tryingWith [clo p, num, str, bool, iden, vec p, expr p, dict p]
    where tryingWith [p] = p
          tryingWith (p:ps) = try p <|> tryingWith ps

vec :: Pos -> Parsec String () InsiType
vec p = between (char '[') (char ']') (Vec <$> insitux p `sepBy` ignorable)

pair :: Pos -> Parsec String () (InsiType, InsiType)
pair p = do
    key <- insitux p
    skipMany (oneOf " ,")
    value <- insitux p
    return (key, value)

dict :: Pos -> Parsec String () InsiType
dict p = between (char '{') (char '}') (Dict <$> pair p `sepBy` ignorable)

expr :: Pos -> Parsec String () InsiType
expr p = between (char '(' >> skipMany ignorable) (skipMany ignorable >> char ')') $ bind p <|> func p <|> eval

func :: Pos -> Parsec String () InsiType
func p = do
    string "function"
    Idn name <- between ignorable ignorable iden
    vars <- iden `sepBy` ignorable
    funcs <- insitux p `sepBy` ignorable
    return $ Binds [(Idn name, Clo "fun" label (vars, funcs))]
        where label = let (n, _) = p in n >>= Just . Left

bind :: Pos -> Parsec String () InsiType
bind p = do
    local <- string "let" <|> string "var"
    skipMany ignorable
    Binds . concat <$> bindings (localToPos local p) `sepBy` ignorable
    where localToPos "var" (_, p) = (Nothing, p)
          localToPos "let"  p    = p

bindings :: Pos -> Parsec String () [(InsiType, InsiType)]
bindings p = do
        Idn name <- iden
        skipMany ignorable
        binding <- insitux $ succArgs p
        return [(Idn name, binding)]
    <|> do -- TODO: proof that tag works inside destructuring
        pvName <- vec $ succNest p
        skipMany ignorable
        pvBinding <- vec (succNest p) <|> str
        return $ together pvName pvBinding
    <|> do
        Dict pdName <- dict $ succNest p
        skipMany ignorable
        Dict pdBinding <- dict $ succNest p
        return $ map (\(k, v) -> (v, toValueOrNullT id $ lookup k pdBinding)) pdName
            where together (Vec n) (Vec b) = zip n b
                  together (Vec n) (Str b) = zipWith (\n' b' -> (n', Str $ b' : "")) n b
                    
eval :: Parsec String () InsiType
eval = do
    exprs <- insitux (Nothing, (0, 0)) `sepBy` many ignorable
    binds <- getState
    return $ Exp exprs

buildAST :: String -> Either ParseError [InsiType]
buildAST = parse (insitux (Nothing, (-1, 0)) `sepBy` ignorable) ""