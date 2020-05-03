-- CS 440, Spring 2019
-- HW 5, recursive descent parser for function call language
--
-- Author: BJ Bloemker A20372354

-- Skeleton file -- fill in missing parts (and delete this comment)

-- This parser is based on the one for Lecture 11.
-- A grammar parser yields a list of trees (possibly singleton)
--

import Data.Char
import Data.List

-- Basic data structures: Parse trees, parser result, parser
-- Note parse result and parser are now parameterized datatypes
-- so our parsers generally are of type Parser [Ptree]
--
data Ptree = VAR String | ID String | FCN String [Ptree]
    deriving (Show, Eq, Read)

data Presult a = FAIL | OK a String   deriving (Show, Eq, Read)
type Parser a = String -> Presult a

-- Language syntax (you may need to rearrange / refactor this)
--
-- expression -> variable | identifier | functionCall
-- variable   -> upper case letter then (alphanumeric or underscore)*
-- identifier -> lower case letter then (alphanumeric or underscore)*
-- functionCall -> identifier ( arguments )
-- arguments -> expression argTail | empty string
-- argTail   -> ',' expression argTail | empty string

-- *** STUB *** need parser routines


var (s) =
    let token = sublist 0 (findStop s) s
        remainder = sublist (findStop s) (length s) s
    in
        if firstIsUpper token && isAllAlphaNumOrUnderscore (drop 1 token) then OK (VAR token) (dropblank remainder)
        else FAIL

identifier (s) =
    let token = sublist 0 (findStop s) s
        remainder = sublist (findStop s) (length s) s
    in
        if firstIsLower token && isAllAlphaNumOrUnderscore (drop 1 token) then OK (ID token) (dropblank remainder)
        else FAIL


func (s) = --at this point we know that s looks like "token(..."
    let token = sublist 0 (findStop s) s
        remainder = sublist (findStop s) (length s) s
        arguments = splitOn ',' (cleanArgs (dropblankback remainder))
        aregumentsClean = mergeArguments (firstParen arguments) (lastParen arguments) (arguments) 
    in
        if not (firstIsLower token) || not (isAllAlphaNumOrUnderscore (drop 1 token)) then FAIL
        else let 
                 results = map expr (map dropblank aregumentsClean)
             in 
                 if FAIL `elem` results then FAIL
                 else OK (FCN token (map ptreeFromPresult results)) ""

expr (s) =
    let token = sublist 0 (findStop s) s
        remainder = sublist (findStop s) (length s) s
    in
        if firstIsUpper token && isAllAlphaNumOrUnderscore (drop 1 token) then var(s)--variable
        else if ((take 1 (dropblank remainder)) == "(") then func(s) --function
        else if firstIsLower token && isAllAlphaNumOrUnderscore (drop 1 token) then identifier(s) --identifier
        else FAIL

----------------------------------------------------------------------
-- UTILITY ROUTINES
--

-- Parse a string but don't save it as a parse tree
--
skip :: String -> Parser [a]
skip want input =
    let found = take (length want) input
        remainder = dropblank (drop (length want) input)
     in
        if want == found then OK [] remainder
        else FAIL

-- Remove spaces (and tabs and newlines) from head of string.
--
dropblank :: String -> String
dropblank = Data.List.dropWhile Data.Char.isSpace

-- *** STUB *** You'll need more routines for yourself

--removes spaces and tabs from the back of a string
dropblankback s = reverse (dropblank (reverse s))


-- Functions to test if all characters in a String are valid
isAllAlphaNumOrUnderscore l = all isAlphaNum (filter (/='_') l)
firstIsUpper s = isUpper (s !! 0)
firstIsLower s = isLower (s !! 0)


--locate where the current token of the string s ends
findStop s =
    let stopSpace = justToInt(findIndex (==' ') s)
        stopOpenParen = justToInt(findIndex (=='(') s)
        stopComma = justToInt(findIndex (==',') s)
        nonZeroList = filter (/=(-1)) [stopSpace,stopOpenParen,stopComma]
    in
        if stopSpace == (-1) && stopOpenParen == (-1) && stopComma == (-1) then length s
        else minimum nonZeroList

--convert Maybe Ints to Ints
justToInt (Just i) = i
justToInt Nothing = (-1)

sublist start end list = take (end - start) (drop start list)


--format function arguments (take away otter most parenthasis)
cleanArgs s =
    let firstChar = take 1 s
        lastChar = take 1 (reverse s)
    in
        if firstChar == "(" && lastChar == ")" then take (length s - 2) (drop 1 s)
        else s 


--splitOn used to seprate list of arguments. Split on s with delimeter del
-- splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn del s = f : splitOn del leftoverFunc 
  where (f, leftoverFunc) = break (== del) (dropWhile (== del) s) 

--counts instances of character c in string s
countOf c s = length (filter (==c) s)

--get Ptree from Presult
ptreeFromPresult (OK a _) = a

--Mergemid: merges middle of list (for function parenthais)
mergemid start end l = (sublist 0 start l) ++ [(intercalate "," (sublist start (end+1) l))] ++ (sublist (end+1) (length l) l)

--firstParen
firstParen l
    | findIndices ('(' `elem`) l == [] = (-1)
    | otherwise = minimum (findIndices ('(' `elem`) l)
--lastParen
lastParen l
    | findIndices (')' `elem`) l == [] = (-1)
    | otherwise = maximum (findIndices (')' `elem`) l)

--mergeArguments: mergest functions in args 
mergeArguments _ (-1) args = args
mergeArguments start end args
    | start == end = args
    | otherwise = mergemid start end args

----------------------------------------------------------------------
-- Concatenation and alternation operators on parsers
--

-- (|>) is an OR/Alternation operator for parsers.
-- It's been made right associative and weaker than &> (and).
-- p1 |> p2 tries p1 and if that fails, then tries p2 instead.
-- It returns the result of the first one that succeeds (or fail
-- if they both fail).
--
infixr 2 |>
(|>) :: Parser a -> Parser a -> Parser a
(p1 |> p2) input =
    case p1 input of
        m1 @ (OK _ _) -> m1 -- if p1 succeeds, just return what it did
        FAIL -> p2 input

-- we look at p1 before p2, so that's why the sublist routine looks for
-- the nonempty possibility , list sublist before looking for empty string


-- (&>) is an AND/Concatenation operator for parsers
-- It's been made right associative and stronger than |> (or).
-- p1 &> p2 tries p1 and then p2 and concatenates their result lists.
-- It fails if either p1 or p2 fails.
--
infixr 3 &>
(&>) :: Parser [a] -> Parser [a] -> Parser [a]
(p1 &> p2) input =
    case p1 input of
        FAIL -> FAIL                    -- p1 fails? we fail
        OK ptrees1 remain1 ->
            case p2 remain1 of          -- run p2 on remaining input
                FAIL -> FAIL            -- p2 fails? we fail
                OK ptrees2 remain2 ->   -- both succeeded
                    OK (ptrees1 ++ ptrees2) remain2

