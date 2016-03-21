import Data.Char


data Category = Atom | Variable | Number | Operator | Complex deriving (Show, Eq)

data Token = Token {
    category :: Category,
    token :: String
}

instance Show Token where
    show (Token {token = token}) = show token


data Term = Term {
    tokenType :: Category,
    name :: String,
    args :: [Term]
}

instance Show Term where
    show (Term {tokenType = tokenType, name = name, args = args}) =
        name ++ if tokenType == Complex then
                    "(" ++ showTermList args ++ ")"
                else []


showTermList [] = []
showTermList (t : []) = show t
showTermList (t : ts) = show t ++ ", " ++ showTermList ts


data Rule = Rule {
    lhs :: Maybe Term,
    rhs :: [Term]
}

instance Show Rule where
    show (Rule { lhs = lhs, rhs = rhs }) =
        show lhs ++ " :- " ++ showTermList rhs


operators = "()[];"
smileyOperator = ":-"
openParen = "("
closeParen = ")"
comma = ","
dot = "."


parse :: [Token] -> [Rule]
parse [] = []
parse ts =
    let (rule, ts') = parseRule ts
    in (rule : parse ts')


parseRule :: [Token] -> (Rule, [Token])
parseRule [] = error "No tokens to parse"
parseRule ts =
    if null ts' || token (head ts') /= dot then
        (rule, ts')
    else
        (rule, tail ts')
    where (rule, ts') = parseRule' ts


parseRule' :: [Token] -> (Rule, [Token])
parseRule' (t : ts)
    | token t == smileyOperator =
        let (terms, ts') = parseTermList ts
        in (Rule {lhs = Nothing, rhs = terms}, ts')
    | otherwise =
        let (term, ts') = parseTerm (t : ts)
            (terms, ts'') =
                if null ts' then
                     error "Syntax error: missing dot (.)"
                else
                    if token (head ts') == smileyOperator then
                        parseTermList $ tail ts'
                    else
                        ([], ts')
        in (Rule {lhs = Just term, rhs = terms}, ts'')


parseTerm :: [Token] -> (Term, [Token])
parseTerm [] = error "No tokens to parse"
parseTerm (t : ts) =
    case category t of
        Variable -> (Term {tokenType = Variable, name = token t, args = []}, ts)
        Number -> (Term{tokenType = Number, name = token t, args = []}, ts)
        Atom -> parseAtom (t : ts)
        Operator -> error "Operator was unexpected here"
    where
        parseAtom (t : ts)
            | null ts || token (head ts) /= openParen =
                (Term {tokenType = Atom, name = token t, args = []}, ts)
            | otherwise = 
                let (args, ts') = parseTermList (ts)
                in (Term {tokenType = Complex, name = token t, args = args}, ts')


parseTermList :: [Token] -> ([Term], [Token])
parseTermList [] = ([], [])
parseTermList (t : ts)
    | token t == openParen = parseTermList ts
    | token t == comma = parseTermList ts
    | token t `elem` [dot, closeParen] = ([], ts) -- TODO this allows . instead ), fix it
    | otherwise =
        let (term, ts') = parseTerm (t : ts)
            (terms, ts'') = parseTermList ts'
        in (term : terms, ts'')


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
    | otherwise = wrap Operator (\c -> not (isLetter c || isSpace c))
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
