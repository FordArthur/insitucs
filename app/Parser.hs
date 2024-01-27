module Parser where

import qualified Data.Map as Map

import Control.Monad (ap)
import Control.Arrow ( Arrow(..) ) 

import Text.Parsec
    ( char,
      digit,
      noneOf,
      oneOf,
      between,
      many1,
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
      Stream, skipMany1, satisfy, choice, endBy, sepEndBy, notFollowedBy, endBy1 )

import Types
    ( InsiType(..), Defs, succArgs, succNest,
      toValueOrNullT, IsScoped)

ignorable :: Parsec String () [Char]
ignorable = many1 (oneOf " ,\n\t\r")

splitArgs :: [InsiType] -> [InsiType] -> [InsiType] -> ([InsiType], [InsiType])
splitArgs args things [] = (reverse args, reverse things)
splitArgs args things (Idn _ ('%':n):xs) = splitArgs (Idn True n':args) (Idn True n':things) xs
    where n' = if null n then "0" else n
splitArgs args things (x:xs) = splitArgs args (x:things) xs

clo :: Parsec String () InsiType
clo = between (string "#(" >> skipMany ignorable) (skipMany ignorable >> char ')') (Clo "lam" . splitArgs [] [] <$> (insitux `sepEndBy` ignorable))
    <|> between (string "@(" >> skipMany ignorable) (skipMany ignorable >> char ')') (Clo "part" . splitArgs [] [] <$> (insitux `sepEndBy` ignorable))
    <|> between (string "(fn" >> skipMany ignorable) (char ')') 
    (
        do 
        cloArgs <- iden `endBy` ignorable;
        funcs <- insitux `sepEndBy` ignorable;
        return . Clo "fun" . first localize $ orFinal cloArgs funcs
    )
    where orFinal ys [] = (init ys, [last ys])
          orFinal ys xs = (ys, xs)
          localize = map (\(Idn _ n) -> Idn True n)

bool :: Parsec String () InsiType
bool = toBool <$> (string "true" <|> string "false")
    where toBool "true"  = Bool True
          toBool "false" = Bool False

null' :: Parsec String () InsiType
null' = string "null" >> return Null

iden :: Parsec String () InsiType
iden = Idn False <$> (notFollowedBy (num <|> str <|> bool <|> null') >> many1 (noneOf " ,\n()[]{}#@;"))

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

insitux :: Parsec String () InsiType
insitux = tryingWith [null', clo, num, str, bool, iden, vec, dict, expr]
    where tryingWith [p] = p
          tryingWith (p:ps) = try p <|> tryingWith ps

vec :: Parsec String () InsiType
vec = between (char '[') (char ']') (Vec <$> insitux `sepEndBy` ignorable)

pair :: Parsec String () (InsiType, InsiType)
pair = do
    key <- insitux
    skipMany (oneOf " ,")
    value <- insitux
    return (key, value)

dict :: Parsec String () InsiType
dict = between (char '{') (char '}') (Dict . Map.fromList <$> pair `sepEndBy` ignorable)

expr :: Parsec String () InsiType
expr = between (char '(' >> skipMany ignorable) (skipMany ignorable >> char ')') $ tryingWith [bind, func, eval]
    where tryingWith [p] = p
          tryingWith (p:ps) = try p <|> tryingWith ps

func :: Parsec String () InsiType
func = do
    string "function"
    Idn _ name <- between ignorable ignorable iden
    vars <- try $ iden `endBy` ignorable
    funcs <- insitux `sepEndBy` ignorable
    let fun = Clo "fun" (vars, funcs)
    return . Binds False fun $ Map.singleton (Idn False name) fun
    
bind :: Parsec String () InsiType
bind = do
    local <- string "let" <|> string "var"
    skipMany ignorable
    let scope = local == "let"
    uncurry (Binds scope) . foldl1 (\(_, b) (l, b') -> (l, b `Map.union` b')) <$> bindings scope `sepEndBy` ignorable

bindings :: IsScoped -> Parsec String () (InsiType, Defs)
bindings scope = do 
        Idn _ name <- iden
        skipMany ignorable
        bind <- insitux
        return (bind, Map.singleton (Idn scope name) bind)
    <|> do 
        pvName <- vec 
        skipMany ignorable
        pvBind <- scoping scope pvName <$> (vec <|> str)
        return (snd $ last pvBind, Map.fromList pvBind)
    <|> do
        Dict pdName <- dict
        skipMany ignorable
        Dict pdBinding <- dict
        let pdBinded = map (\(k, v) -> (v, toValueOrNullT id $ Map.lookup k pdBinding)) $ Map.toList pdName
            in return (snd $ last pdBinded, Map.fromList pdBinded)
        where scoping s (Vec n) (Vec b) = zipWith (\(Idn _ n ) b -> (Idn s n, b)) n b
              scoping s (Vec n) (Str b) = zipWith (\(Idn _ n') b' -> (Idn s n', Str $ b' : "")) n b

eval :: Parsec String () InsiType
eval = Exp <$> insitux `sepEndBy` ignorable

buildAST :: String -> Either ParseError [InsiType]
buildAST = parse (many ignorable >> insitux `sepEndBy` many (oneOf " ,\n\t\r")) ""