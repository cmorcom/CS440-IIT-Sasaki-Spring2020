module Notes where
import Data.Char
import Data.List

{-
add_strs :: String -> String -> String
add_strs a b = a++b

data Ptree =
    Empty                           -- The empty parse tree
    | Id String                     -- Identifiers
    | Call String [Ptree]           -- Function call, list of arguments
    | Exp Ptree Ptree               -- For Term/Term_tail
    | Term Ptree Ptree              -- For Factor/Factor_Tail
    | Ttail Symbol Ptree Ptree      -- for Symbol Term Ttail
    | Ftail Symbol Ptree Ptree      -- Symbol Factor Ftail
    | Negative Ptree                -- For - factor
    deriving (Eq, Show, Read)

--
-- As before, symbols are just characters
--
type Symbol = Char
type Input  = [Symbol]

--
-- Some basic symbols
--
comma  = ',' :: Symbol 
lparen = '(' :: Symbol
minus  = '-' :: Symbol
plus   = '+' :: Symbol
rparen = ')' :: Symbol
star   = '*' :: Symbol 
equals = '=' :: Symbol

-- A generic parser returns Maybe a value and leftover input.
-- Most of the parsers are of type Parser Ptree.
--
type Parser t = Input -> Maybe (t, Input)

type Term = [Char]
type Equation a = [Char] -> (Term, Term)


parse_equation :: String
parse_equation [] = Nothing
parse_equation input = string_split(dropSpaces input, '=')



---------- UTILITIES --------------------------------------------------
--
-- The bind routine lets us take a Just val and run a function on the val.
-- If given Nothing instead, bind also yields Nothing.
--
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing f = Nothing
bind (Just val) f = f val

--
-- The fails routine lets you call a function() if given Nothing; if
-- given Just val, the fails routine just yields that.
--
fails :: Maybe a -> (() -> Maybe a) -> Maybe a
fails Nothing f = f()
fails ok _ = ok

--
-- make_tail builds an expr using a term and term tail; it builds a term
-- using a factor and factor tail.  If the tail is empty, make_tail just
-- returns the given term or factor.  This optimization reduces the
-- number of skinny paths through the parse tree, which becomes shorter.
--
make_tail :: (Ptree -> Ptree -> Ptree) -> Ptree -> Ptree -> Ptree
make_tail _ ptree Empty = ptree
make_tail build ptree tailtree =
    build ptree tailtree

-- Remove initial whitespace and look for an identifier string.  If found,
-- return the string (and leftover input).
--
getId :: Parser String
getId [] = Nothing
getId (h:input1)
    | isLetter h =
        let (idtail, input2) = span (\c -> isAlphaNum c || c == '_') input1
        in Just (h:idtail, input2)
    | otherwise = Nothing



--
parse_equation :: String -> [String]
parse_equation [] = []
parse_equation input = string_split '=' (dropSpaces input)

string_split :: Char -> String -> [String]
string_split _ "" = [];
string_split delim input = abreak (== delim) input

input = "f(a(b(c(d)))=g(a(b(d)c))" 
-}
-- drop initial whitespace
dropSpaces x = dropWhile isSpace x
type Term = [Char]
type Equation a = [Term]
--type Equations a = Equation a | Equations a

dropInnerSpaces x = [c | c <- x, c /= ' ']
parse_equation :: String -> [Term]
parse_equation [] = []
parse_equation input =
    let tmp = break (== '=') (dropInnerSpaces input)
    in [Just( Term fst(tmp)), Just( Term tail(snd(tmp)))]
--a = do [x | x <- i] where i = [[c] | c <- (dropInnerSpaces "f(a(b(c(d))) = g(a(b(d)c))"), c /= '=']