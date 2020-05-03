-- Homework 4 part 1
-- CS 440, Spring 2019
--
-- Match regular expressions using backtracing search

-- Simple Regular Expressions with exp1 exp2, exp1 | exp2,
-- null string (epsilon), $ (end of string), dot (any single character), 
-- optional expr (? exp), and Kleene star

data RegExp = Rnull                 -- ∊ empty string epsilon
            | Rend                  -- $ at end of string?
            | Rany                  -- . any one character
            | Rch Char              -- a specific character
            | Ror RegExp RegExp     -- | alternation
            | Rand RegExp RegExp    -- concatenation
            | Ropt RegExp           -- ? optional expression
            | Rstar RegExp          -- * Kleene star
            deriving (Eq, Show)

-- A match result OK str1 str2 means that str1 matched the expression and
-- str2 was leftover.  Note str1 ++ str2 should be the string passed to match.
--
data Mresult = FAIL | OK String String deriving (Eq, Show)


-- Matching takes a regular expression and a string and either fails or
-- returns the string that matched and the string that was leftover.
--
match :: RegExp -> String -> Mresult

-- *** You fill in STUBS below ***

-- Matching empty string always succeeds, with empty string as the matched string
match Rnull str = OK "" str

-- Match Rend str succeeds iff we're at the end of the string (str is null)
match Rend str
    | str == "" = OK str str
    | otherwise = FAIL

-- Match Rany string matches the first character of the string (failing if string is null)
match Rany str 
    | length str == 0 = FAIL
    | otherwise = OK (take 1 str) (drop 1 str)

-- Match a character succeeds iff the string begins with the character
match (Rch ch1) str
    | [ch1] == take 1 str = OK (take 1 str) (drop 1 str)
    | otherwise = FAIL


-- Match (Ror r1 r2) string matches r1 against string and also r2 against string
-- and fails if both fail, returns the successful result if exactly one succeeds,
-- and returns the result with the longer match if both succeed
--
match (Ror rexp1 rexp2) str
    | (match rexp1 str) == FAIL && (match rexp2 str) == FAIL = FAIL
    | matchLength (match rexp1 str) > matchLength (match rexp2 str) = (match rexp1 str)
    | otherwise = (match rexp2 str)

-- Match (Rand r1 r2) string matches r1 and (if it succeeds) matches r2 against
-- the string leftover from r1.  If r2 succeeds then the Rand returns the
-- concatenation of the matches from r1 and r2.
--
match (Rand rexp1 rexp2) str
    | (match rexp1 str) == FAIL = FAIL
    | (match rexp2 (matchOKStrTwo(match rexp1 str))) == FAIL = FAIL
    | otherwise = (OK (matchOKStrOne((match rexp1 str)) ++ matchOKStrOne((match rexp2 (matchOKStrTwo(match rexp1 str))))) (matchOKStrTwo(match rexp2 (matchOKStrTwo(match rexp1 str)))))

-- Matching an optional expression is like matching (rexp | null)
match (Ropt rexp) str 
    | (match rexp str) == FAIL = (OK "" str) -- No way to fail an optional
    | otherwise = (match rexp str) 

-- matching rexp* is like matching ((rexp rexp*) | null), but to avoid infinite
-- recursion, if rexp matches null, we stop with the base case (matching null)
-- We also stop with the base case if rexp fails to match.
--
match (Rstar rexp) str = OK (looper (Rstar rexp) str) (drop (length (looper (Rstar rexp) str)) str)


looper (Rstar rexp) str 
    | (match rexp str) /= FAIL && (match rexp str) /= nullcase = matchOKStrOne(match rexp str) ++ (looper (Rstar rexp) (drop 1 str))
    | otherwise = ""

----------------------------------------------------------------------
-- UTILITY FUNCTIONS

-- extend match1 takes a successful search result (OK match2 left2) and returns a
-- successful search result with the concatenation of match1 and match2.
-- (extend match1 on a failed result also fails.)
--
extend match1 (OK str1 str2) = OK (match1 ++ str1) str2
extend match1 _ = FAIL

-- mkAnd string = reg expr that searches for the characters of the string in sequence
--
mkAnd (c : "") = Rch c
mkAnd (c : cs) = Rand (Rch c) (mkAnd cs)

-- mkOr string = reg expr that matches any character of the string
--
mkOr (c : "") = Rch c
mkOr (c : cs) = Ror (Rch c) (mkOr cs)

matchLength (OK str1 str2) = length str1
matchLength FAIL = -1

matchOKStrOne (OK str1 str2) = str1
matchOKStrTwo (OK str1 str2) = str2

nullcase = OK "" ""
