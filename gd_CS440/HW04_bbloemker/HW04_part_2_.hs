-- Homework 4 part 2
-- Match regular expressions using nondeterministic search
-- In contrast to part 1, here, matching returns a list of all the
-- possible successful results.

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

-- A match result  [OK str1 str2] is a list of success pairs where str1 is
-- the prefix that matched and str2 is the suffix that was leftover.
-- A list of length 0 indicates no successful matches.  A list of length > 1
-- indicates that multiple matches exist.  (E.g., matching ("a" | "ab") with "abc"
-- returns two results, one matching "a" (and leaving "bc") and one matching
-- "ab" and leaving "c".  For each OK str1 str2 result, str1 ++ str2 should
-- be the original string passed to match.
--
data Mresult = OK String String deriving (Eq, Show)
type Mresults = [Mresult]


-- Matching takes a regular expression and a string and either fails or
-- returns the string that matched and the string that was leftover.
--
match :: RegExp -> String -> Mresults

-- *** You fill in STUBS below.  Many are basically the same as in the
-- *** backtracking search case.

-- Matching empty string always succeeds, with empty string as the matched string
match Rnull str = [OK "" str]

-- Match Rend str succeeds iff we're at the end of the string (str is null)
match Rend str 
    | str == "" = [OK str str]
    | otherwise = []

-- Match Rany string matches the first character of the string (failing if string is null)
match Rany str
    | length str == 0 = []
    | otherwise = [OK (take 1 str) (drop 1 str)]

-- Match a character succeeds iff the string begins with the character
match (Rch ch1) str
    | [ch1] == take 1 str = [OK (take 1 str) (drop 1 str)]
    | otherwise = []

-- Match (Ror r1 r2) string matches r1 against string and also r2 against string
-- and combines the results from both matches
--
match (Ror rexp1 rexp2) str = (match rexp1 str) ++ (match rexp2 str) 

-- Match (Rand r1 r2) string first matches r1; for every successful result,
-- it follows that particulra result with a match against r2.  All the
-- results (across all the results of r1) are collected and returned.
--
match (Rand rexp1 rexp2) str = [] -- STUB

-- Matching an optional expression is like matching (rexp | null)
match (Ropt rexp) str
    | (match rexp str) == [] = [(OK "" str)] -- No way to fail an optional
    | otherwise = (match rexp str) 

-- matching rexp* is like matching ((rexp rexp*) | null), but the
-- nondeterministic treatment of or means that we combine all the
-- results of successful matches for exp^n for all n.  (exp^n being
-- n concatenations of exp with epsilon for exp^0.)
--
match (Rstar rexp) str = [OK (looper (Rstar rexp) str) (drop (length (looper (Rstar rexp) str)) str)]


looper (Rstar rexp) str 
    | (match rexp str) /= [] && (match rexp str) /= nullcase = matchOKStrOne(match rexp str) ++ (looper (Rstar rexp) (drop 1 str))
    | otherwise = []

----------------------------------------------------------------------
-- UTILITY FUNCTIONS

matchLength [(OK str1 str2)] = length str1
matchLength [] = -1

matchOKStrOne [(OK str1 str2)] = str1
matchOKStrTwo [(OK str1 str2)] = str2

nullcase = [OK "" ""]