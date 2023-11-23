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
      Stream, skipMany1, satisfy, choice, endBy )

import Types
    ( InsiType(Exp, Bool, Num, Binds, Clo, Vec, Str, Idn, Dict),
      toValueOrNullT )

ignorable :: Parsec String () [Char]
ignorable = many1 (oneOf " ,\n")

accepted :: Parsec String () [Char]
accepted = many1 (noneOf " ,()[]{}#@;")

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
expr = between (char '(' >> skipMany ignorable) (skipMany ignorable >> char ')') $ bind <|> func <|> eval 

bind :: Parsec String () InsiType -- will get declarified
bind = do
    isLocal <- string "let" <|> string "var"
    skipMany ignorable
    binds <- concat <$> bindings (isLocal == "let") `sepBy` ignorable
    return $ Binds binds

func :: Parsec String () InsiType
func = do
    string "function"
    name <- between ignorable ignorable iden
    vars <- iden `sepBy` ignorable
    funcs <- insitux `sepBy` ignorable
    return $ Binds [(name, Clo "fun" (vars, funcs))]

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
        return $ map (\(k, v) -> (v, toValueOrNullT id $ lookup k pdBinding)) pdName
            where together (Vec n) (Vec b) = zipWith (\n' b' -> (scopeIf local n', b')) n b
                  together (Vec n) (Str b) = zipWith (\n' b' -> (scopeIf local n', Str $ b' : "")) n b
                  scopeIf True (Idn name) = Idn $ '\"' : name ++ "\""
                  scopeIf _    name = name 
                  
eval :: Parsec String () InsiType
eval = do
    exprs <- insitux `sepBy` many ignorable
    binds <- getState
    return $ Exp exprs

buildAST = parse (insitux `sepBy` ignorable) ""