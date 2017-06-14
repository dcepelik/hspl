> module Parser (Term (Atom, Number, Variable, Compound), Rule (Rule), Program, parse, term, program) where

> import Data.Char
> import Data.List
> import Control.Monad
> import Control.Applicative

We begin by defining Prolog data structures which we will construct from the source code by the
means of parsing.

> data Term = Atom String
>           | Number Int
>           | Variable String
>           | Compound String [Term]

A Rule is a Prolog rule: a head term with body terms.

> data Rule = Rule Term [Term]

A Program is simply a list of rules in the program, that's it.

> type Program = [Rule]

Note we didn't introduce any special structure for Prolog lists. Prolog lists are just Compound
terms with `.' main functor, ie.

	[1, f(X), alfa, beta, [ 2, 3]|X]

is simply

	.(1, .(f(X), .(alfa, .(beta, .(.(2, (.3, [])), X)))))

and everything works in a general fashion without special constructs.

Terms may be pretty-printed:
           
> instance Show Term where
>     show (Atom a) = a
>     show (Number n) = show n
>     show (Variable x) = x
>     show term@(Compound f args) =
>		if f == "." then showList' term
>		else f ++ "(" ++ (intercalate ", " [ show arg | arg <- args ]) ++ ")"

Rules, too, may be pretty-printed.

> instance Show Rule where
>     show (Rule head body) = (show head) ++ (if body /= [] then " :- " else "") ++ intercalate ", " (map show body) ++ "."

showList' is a specialized function to print a Prolog list. (List are just Compound terms with
syntactic sugar, see above.)

> showList' :: Term -> String
> showList' list = "[" ++ showList'' list ++ "]"

showList'' just prints the inside of a list (ie. whatever comes between `[' and `]'.

> showList'' :: Term -> String
> showList'' (Compound "." [head, Variable v]) = (show head) ++ "|" ++ v
> showList'' (Compound "." [head, tail]) = (show head) ++ (if tail /= Atom "[]" then ", " else "") ++ showList'' tail
> showList'' (Atom "[]") = ""

The comparison of terms is "dumb", ie. we compare the string representations.

> instance Eq Term where
>     (==) a b = (show a) == (show b)

Parser is a wrapper around a String -> [(a, String)] function, which, given input string, outputs
a list of (a, String) pairs: a is the result of the parsing process, String is the unprocessed
input.

> data Parser a = Parser (String -> [(a, String)])

Next, we make Parser a Monad. We have to, therfore, make it Functor and Applicative, too. We have
to make it MonadPlus because we want it to be Alternative.

> instance Functor Parser where
>     fmap = liftM

> instance Applicative Parser where
>     pure v = Parser (\input -> [(v, input)])
>     (<*>) = ap

The important part is the >>= operator which allows us to sequence parsers in a do-block. This way,
the result of the application of a Parser may be bound to a variable in the do-block, and then
used in subsequent computations.

> instance Monad Parser where
>     p >>= f = Parser (
>         \cs -> concat [parse' (f val) cs' | (val, cs') <- parse' p cs])
>     return = pure

We are about to make Parser an Alternative instance. This way, we can combine parsers using <|>.
The result of such an aplication is the list of all possible ways one can parse the input using
either parser.

`many' and `some' allow for construction of "repeated" parsers, such as to a parser to parse
several terms in a row.

> instance MonadPlus Parser where
>     mzero = Parser (\input -> [])
>     mplus p q = Parser (\input -> parse' p input ++ parse' q input) 

> instance Alternative Parser where
>     empty = mzero
>     (<|>) = mplus
>     many p = some p `mplus` return []
>     some p = do
>         a <- p
>         as <- many p
>         return (a:as)

Apply a Parser to some input string, return the data and the new string.

> parse' :: Parser a -> String -> [(a, String)]
> parse' (Parser f) input = f input

Apply a Parser to some input string, but only return the data and discard the new state.

> parse :: Parser a -> String -> a
> parse (Parser f) input = fst $ head $ f input

Now, we introduce several primitive parsers, which we'll use to build more complicated parsers.

The `char' parser just consumes one given character from the input if possible.

> char :: Char -> Parser Char
> char c = sat (c ==)

The `string' parser consumes whatever string it is given.

> string :: String -> Parser String
> string "" = return ""
> string (c:cs) = do
>     char c
>     string cs
>     return (c:cs)

`item' returns a single character.

> item :: Parser Char
> item = Parser (\input -> case input of
>     [] -> []
>     (c:cs) -> [(c, cs)])

This `sat' parser eats a character maching given (Char -> Bool) predicate.

> sat :: (Char -> Bool) -> Parser Char
> sat pred = do
>     c <- item
>     if pred c then return c else mzero

`sat2' is a more general version of `sat' which is given two predicates; one for the initial
character of the string, one for the other characters of the string it parses.

> sat2 :: (Char -> Bool) -> (Char -> Bool) -> Parser String
> sat2 initChar insideChar = do
>     c <- sat initChar
>     cs <- many (sat insideChar)
>     return (c:cs)

`space' happens to read a consecutive whitespace from the input if present.

> space :: Parser String
> space = many (sat isSpace)

`triml', given a parser, constructs a parser that ignores whitespace prior to proceeding.

> triml :: Parser a -> Parser a
> triml p = do
>     space
>     r <- p
>     return r

`sepBy' returns a parser to parse a list of things parsed by the first parser it is given,
separated by strings parsed by the second parser.

> sepBy :: Parser a -> Parser b -> Parser [a]
> sepBy p sep = do
>     a <- p
>     as <- many (do {sep; p})
>     return (a:as)

Next, we'll already introduce parsers for the Prolog language. Here, `dot' and `comma' are trivial
parsers for a dot (.) and a comma (,).

> dot = triml $ char '.'
> comma = triml $ char ','

`argList1' is an argument-list parser. It requires at least one argument to be present.

> argList1 :: Parser a -> Parser [a]
> argList1 p = p `sepBy` comma

`argList' is a relaxed version of `argList1', making no assumptions about the number of arguments
in the arglist, ie. the arglist may as well be empty.

> argList :: Parser a -> Parser [a]
> argList p = argList1 p <|> return []

This predicate tells wheter a given character is considered a valid identifier character.

> isIdentChar :: Char -> Bool
> isIdentChar c = isAlphaNum c || c == '_'

Parser for the `:-' (here called "smiley") operator.

> smiley = triml $ string ":-"

Now for some more interesting parsers!

`variable' parses a Prolog variable. It uses `sat2' to do that, giving predicate isUpper for the
initial character of the variable name and isIdentChar for the consecutive characters.

> variable :: Parser Term
> variable = fmap Variable $ sat2 isUpper isIdentChar

`atom' parses, well, an atom. The predicates are here given as lambdas.

> atom :: Parser Term
> atom = fmap Atom $ sat2 (\c -> isLower c || c == '_') (\ c -> isIdentChar c || c == '_')

`number' parser follows in the same fashion.

> number :: Parser Term
> number = fmap (Number . read) (some (sat isNumber))

`compound' is a bit more interesting. We use a do block and we sequence the operations in such
a way that we first parse an atom, which is the functor name, then a left paren (not preceeded by
a space), then the (potentially empty) arglist, and a trailing paren.

> compound :: Parser Term
> compound = fmap (uncurry Compound) $ do
>     Atom f <- triml atom
>     char '(' -- don't triml here: space before `(' not allowed
>     args <- argList1 $ triml term
>     triml $ char ')'
>     return (f, args)

`emptyList' is just a name given to Atom "[]".

> emptyList :: Term
> emptyList = Atom "[]"

The `term' parser is the most important one (in a sense); it "decides" what category of token
will be parsed from the input. It's the reason we wanted to have Parser as an Applicative instance.

So, term may be a list (a special case of a compound term), or a compound term, or an atom,
variable or number.

> term :: Parser Term
> term = list <|> compound <|> atom <|> variable <|> number

To parse a rule means to parse head term first, then the body terms. A dot is required at the end.

> rule :: Parser Rule
> rule = fmap (uncurry Rule) $ do
>     ruleHead <- term
>     body <- (do {smiley; argList term} <|> return [])
>     dot
>     return (ruleHead, body)

Parsing a program is simply parsing of a lot of rules.

> program :: Parser Program
> program = do
>     space
>     rs <- many rule
>     return rs

Now for the parsers of Prolog lists. These are simple, too, but potentially not as clear as the
other parsers. Even though Prolog lists are internally just special compound terms, their syntax
is substantially different.

The list parser here initializes the parsing of a list, requiring it to begin with a `['. Then
a matching `]' may follow, resulting in the empty list, or the contents of a list is parsed with
list'.

> list :: Parser Term
> list = do
>	char '['
>	do { triml $ char ']'; return emptyList} <|> list'

Here `list'' itself calls two helpers, parseHead and parseTail, to parse a single item (the head)
of the list, and several "tail" items. See:

> list' :: Parser Term
> list' = fmap (uncurry Compound) $ do
>	head <- listHead
>	tail <- listTail
>	return (".", [head, tail])

This is a method to parse the end of a list.

> list'' :: Parser Term
> list'' = do
>	triml $ char ']'
>	return emptyList

This is the already explained `listHead' function. It either parses a list head, or it parses the
end of a list.

> listHead :: Parser Term
> listHead = triml term <|> list''

Here `listTail' is a bit more interesting again, it enumerates all the options there are after
the head of the list has been parsed: we may either get a pipe `|', a comma `,' or a the end
of the list.

Also, after the pipe, either a list may follow or a single variable.

> listTail :: Parser Term
> listTail = do { triml $ char '|'; triml list <|> listTailVar } <|> do { triml $ char ','; list' } <|> list''

Parse "tail variable", that is, the variable X in the expression `[a, b|X]'.

> listTailVar :: Parser Term
> listTailVar = do
>	var <- triml variable
>	char ']'
>	return var
