import Data.Char


data Category = Atom | Variable | Number | Complex deriving Show

data Token = Token {
    category :: Category,
    token :: String
}

instance Show Token where
    show (Token category token) = show token


data Term = Term {
    tokenType :: Category,
    name :: String,
    args :: [Term]
} deriving Show

data Rule = Rule {
    head :: Term,
    body :: [Term]
} deriving Show


operators = "()[];"
smileyOperator = ":-"


-- parse :: String -> [Rule]


tokens :: String -> [Token]
tokens [] = []
tokens (c : cs)
    | isSpace c = tokens cs -- eat all whitespace
    | otherwise =
        let (token, cs') = nextToken (c : cs)
        in token : tokens cs'


nextToken :: String -> (Token, String)
nextToken [] = error "There are no tokens in an empty string"
nextToken (c : cs)
    | c `elem` operators = (Token {category = Atom, token = [c]}, cs)
    | isUpper c || c == '_' = wrap Variable isValidIdentifierChar
    | isLower c = wrap Atom isValidIdentifierChar
    | isNumber c = wrap Number isNumber
    | otherwise = wrap Atom (\c -> not (isLetter c || isSpace c))
    where
        wrap category charFilter =
           let (acc, cs') = accumulate (c : cs) charFilter
           in (Token {category = category, token = acc}, cs')
        isValidIdentifierChar c = isAlphaNum c || c == '_'


accumulate :: String -> (Char -> Bool) -> (String, String)
accumulate [] _ = ([], [])
accumulate (c : cs) charFilter
    | charFilter c =
        let (acc, cs') = accumulate cs charFilter
        in (c : acc, cs')
    | otherwise = ([], c : cs)
