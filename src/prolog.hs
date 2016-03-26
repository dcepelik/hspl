import Data.Char
import Control.Monad
import Control.Applicative

{- data structures -}

data Term = Atom String
          | Number Int
          | Variable String
          | Complex String [Term]
          deriving Show

data Rule = Rule (Maybe Term) [Term]
            deriving Show

{- Parser algebra -}

data Parser a = Parser (String -> [(a, String)])

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure v = Parser (\input -> [(v, input)])
    (<*>) = ap

instance Monad Parser where
    p >>= f = Parser (
        \cs -> concat [parse (f val) cs' | (val, cs') <- parse p cs])
    return = pure

instance MonadPlus Parser where
    mzero = Parser (\input -> [])
    mplus p q = Parser (\input -> parse p input ++ parse q input) 

instance Alternative Parser where
    empty = mzero
    (<|>) = mplus
    many p = some p `mplus` return []
    some p = do
        a <- p
        as <- many p
        return (a:as)

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) input = p input

{- simple parsers: building blocks for other parsers -}

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do
    char c
    string cs
    return (c:cs)

item :: Parser Char
item = Parser (\input -> case input of
    [] -> []
    (c:cs) -> [(c, cs)])

sat :: (Char -> Bool) -> Parser Char
sat pred = do
    c <- item
    if pred c then return c else mzero

sat2 :: (Char -> Bool) -> (Char -> Bool) -> Parser String
sat2 initChar insideChar = do
    c <- sat initChar
    cs <- many (sat insideChar)
    return (c:cs)

space :: Parser String
space = many (sat isSpace)

triml :: Parser a -> Parser a
triml p = do
    space
    r <- p
    return r

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = do
    a <- p
    as <- many (do {sep; p})
    return (a:as)

list1 :: Parser a -> Parser [a]
list1 p = p `sepBy` comma

list :: Parser a -> Parser [a]
list p = list1 p <|> return []

{- Prolog character classes and operator parsers -}

dot = triml $ char '.'
comma = triml $ char ','

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || isSymbol c

smiley = triml $ string ":-"

{- Prolog language construct parsers -}

variable :: Parser Term
variable = fmap Variable $ sat2 isUpper isIdentChar

atom :: Parser Term
atom = fmap Atom $ sat2 (\c -> isLower c || c == '_') isIdentChar

number :: Parser Term
number = fmap (Number . read) (some (sat isNumber))

complex :: Parser Term
complex = fmap (uncurry Complex) $ do
    Atom f <- triml atom
    char '(' -- don't triml here: space before `(' not allowed
    args <- list1 $ triml term
    triml $ char ')'
    return (f, args)

term :: Parser Term
term = complex <|> atom <|> variable <|> number

rule :: Parser Rule
rule = fmap (uncurry Rule) $ do
    ruleHead <- (fmap Just term <|> return Nothing)
    body <- (do {smiley; list term} <|> return [])
    dot
    return (ruleHead, body) -- TODO `:-.` ~~~> Rule Nothing [], do I mind?
