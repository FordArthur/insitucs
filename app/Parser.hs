module Parser where

import Control.Monad (ap)
import Control.Arrow ( Arrow(second, (***)) ) 

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
    ( InsiType(..), Label, Defs, succArgs, succNest,
      toValueOrNullT)

enumSepEndBy1 :: a -> (a -> a) -> (a -> Parsec s u b) -> Parsec s u sep -> Parsec s u [b]
enumSepEndBy1 start next p sep = 
    do x <- p start
        ; do { _ <- sep
             ; xs <- enumSepEndBy (next start) next p sep
             ; return (x:xs)
             }
             <|> return [x]

enumSepEndBy :: a -> (a -> a) -> (a -> Parsec s u b) -> Parsec s u sep -> Parsec s u [b]
enumSepEndBy start next p sep = enumSepEndBy1 start next p sep <|> return []

ignorable :: Parsec String () [Char]
ignorable = many1 (oneOf " ,\n\t\r")

accepted :: Parsec String () [Char]
accepted = notFollowedBy (num <|> str <|> bool <|> null') >> many1 (noneOf " ,\n()[]{}#@;")

splitArgs :: [InsiType] -> [InsiType] -> [InsiType] -> ([InsiType], [InsiType])
splitArgs args things [] = (reverse args, reverse things)
splitArgs args things (Idn l ('%':n):xs) = splitArgs (Idn l n':args) (Idn l n':things) xs
    where n' = if null n then "0" else n
splitArgs args things (x:xs) = splitArgs args (x:things) xs

clo :: Label -> Parsec String () InsiType
clo (_, p') = between (string "#(" >> skipMany ignorable) (skipMany ignorable >> char ')') (Clo "lam" p . splitArgs [] [] <$> (insitux p `sepEndBy` ignorable))
    <|> between (string "@(" >> skipMany ignorable) (skipMany ignorable >> char ')') (Clo "part" p . splitArgs [] [] <$> (insitux p `sepEndBy` ignorable))
    <|> between (string "(fn" >> skipMany ignorable) (char ')') 
    (
        do 
        cloArgs <- iden p `endBy` ignorable;
        funcs <- enumSepEndBy p succArgs insitux ignorable;
        return $ Clo "fun" p $ orFinal cloArgs funcs
    )
    where p = (Nothing, p')
          orFinal ys [] = (init ys, [last ys])
          orFinal ys xs = (ys, xs)

bool :: Parsec String () InsiType
bool = toBool <$> (string "true" <|> string "false")
    where toBool "true"  = Bool True
          toBool "false" = Bool False

null' :: Parsec String () InsiType
null' = string "null" >> return Null

iden :: Label -> Parsec String () InsiType
iden l = Idn l <$> accepted

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

insitux :: Label -> Parsec String () InsiType
insitux p = tryingWith [null', clo p', num, str, bool, iden p', vec p', dict p', expr p']
    where tryingWith [p] = p
          tryingWith (p:ps) = try p <|> tryingWith ps
          p' = succNest p

vec :: Label -> Parsec String () InsiType
vec p = between (char '[') (char ']') (Vec <$> enumSepEndBy p succArgs insitux ignorable)

pair :: Label -> Parsec String () (InsiType, InsiType)
pair p = do
    key <- insitux p
    skipMany (oneOf " ,")
    value <- insitux (succArgs p)
    return (key, value)

dict :: Label -> Parsec String () InsiType
dict p = between (char '{') (char '}') (Dict <$> enumSepEndBy p (succArgs . succArgs) pair ignorable)

expr :: Label -> Parsec String () InsiType
expr p = between (char '(' >> skipMany ignorable) (skipMany ignorable >> char ')') $ tryingWith p [bind, func, eval]
    where tryingWith q [p] = p q
          tryingWith q (p:ps) = try (p q) <|> tryingWith q ps

func :: Label -> Parsec String () InsiType
func p = do
    string "function"
    Idn _ name <- between ignorable ignorable $ iden p
    vars <- try $ iden p `endBy` ignorable
    funcs <- enumSepEndBy p succArgs insitux ignorable
    return $ Binds [(Idn p name, Clo "fun" p (vars, funcs))]

bind :: Label -> Parsec String () InsiType
bind p = do
    local <- string "let" <|> string "var"
    skipMany ignorable
    Binds . concat <$> enumSepEndBy p (succArgs . succArgs) (bindings . localToLabel local) ignorable
    where localToLabel "var" (_, p) = (Nothing, p)
          localToLabel "let"  l     = l

bindings :: Label -> Parsec String () Defs
bindings p = do
        Idn _ name <- iden p
        skipMany ignorable
        let p' = succArgs p
        binding <- insitux p'
        return [(Idn (endPos p) name, markClo name (endPos p') binding)]
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
                  markClo n (_, p) (Clo t _ (vs, cs)) = Clo t l (map (\(Idn _ v) -> Idn l v) vs, map useLabel cs)
                    where l = (Just n, p)
                          useLabel (Idn _ i) = Idn l i
                          useLabel (Exp e) = Exp $ map useLabel e
                          useLabel (Dict d) = Dict $ map (useLabel *** useLabel) d
                          useLabel (Vec v) = Vec $ map useLabel v
                          useLabel ix      = ix
                  markClo _ _ ix          = ix
                  endPos = second (const Nothing)

eval :: Label -> Parsec String () InsiType
eval p = do
    exprs <- insitux p `sepEndBy` ignorable
    return $ Exp exprs

buildAST :: String -> Either ParseError [InsiType]
buildAST = parse (many ignorable >> insitux (Nothing, Just (-1, 0)) `sepEndBy` many (oneOf " ,\n\t\r")) ""