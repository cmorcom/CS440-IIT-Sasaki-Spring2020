-- CS 440, Spring 2019
-- Final project: Parse and solve unification equations on simple terms
--
-- framework version 2019-04-09

-- instructions: This is some scaffolding for the final project; most of
-- it has been left out.  Some stub code has been inserted so that this
-- will compile, but it won't do anything.  Please create definitions for
-- the functions mentioned in the comments below (plus any other functions
-- you need, of course).

import Data.Char
import Data.List

----------------------------------------------------------------------
-- External functions (intended for user; include in Final Project)
--
-- psp string -- ("Parse, solve print"): Parse string as a problem,
--     use unification to find solution, pretty print out the
--     substitution solution.  (Return failure message if parse
--     or unification fails.)
--
-- parse_solve string -- similar to psp string but returns the
--     substitution data structure (instead of pretty printing it).
--

-- Main internal functions (include in Final Project)
--
-- problem string -- parse string as a unification problem with
--    result Nothing or Just (problem, remaining input).
--
-- DONE term string -- parse string as a term with result Nothing
--    or  Just (term, remaining input).
--
-- unify equations -- solve a list of equations through unification and
--     return Nothing or Just(a list of substitution pairs).
--
-- DONE substitute var1 term1 term2 = result of substitution of term1 for var1
--     in term2
--

----------------------------------------------------------------------
-- Data Structures and type declarations
--

-- Types for Unification problems, equations, and substitutions
--
type Problem = [Equation]
type Equation = (Term, Term)
type Subst = (Variable, Term)


-- Types/datatypes for Terms (variables, id's, constants, and function calls)
--
type Variable = String
type Identifier = String

data Term = VAR Variable | ID Identifier | CONST Int
    | FCN Identifier [Term] deriving (Show, Eq, Read)


-- Type for parsers; note we're using Maybe(a, String) instead of Presult a.
--
type Parser a = String -> Maybe(a, String)


----------------------------------------------------------------------
-- Print and solve functions (top-level functions intended for user)
--
-- psp :: String (for problem) -> String (for solution) output can be
--     "Fail" or pretty print of substitutions solutions
--
psp :: String -> String
psp input = case problem input of -- try parsing a problem
    Nothing -> "Parse failed"
    Just(equations, _) -> case unify equations of
        Nothing -> "Unification failed"
        Just substitutions -> ------ pretty print substitutions -----
            "" -- <- stub code

-- parse_solve string tries to parse the input as a problem and then run unify
-- on the result.  (Failures along the way makes use yield Nothing.)
--
parse_solve :: String -> Maybe [Subst]
parse_solve input = case problem input of
    Nothing -> Nothing
    Just(equations, _) -> unify equations


----------------------------------------------------------------------
-- Parsers for Unification Problems and Equations
--
-- Syntax: ($ means end of input)
--     Unification_Problem -> { Equation EquationTail } $
--     Equation -> Term = Term
--     EquationTail -> , Equation EquationTail | ε

    ---------- (unification problem parser code) ----------
problem input = Nothing -- <--- stub code

----------------------------------------------------------------------
-- Parses for Terms (and variables, identifiers, constants, function calls)
--
-- Syntax:
--     Term -> Variable | Identifier | FunctionCall
--     Variable   -> [A-Z] [A-Z0-9_]*   -- upper case letters, digits, underscore
--     Identifier -> [a-a] [a-a0-9_]*   -- lower case letters, digits, underscore
--     Constant -> [0-9]+
--     FunctionCall -> Identifier ( Arguments )
--     Arguments -> Term ArgTail | empty string
--     ArgTail   -> ',' Term ArgTail | empty string


    ---------- (term parser code) ----------

----------------------------------------------------------------------
-- Concatenation and alternation operators on parsers
-- As before, we use &> and |> as AND / OR combinators on parsers
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
        m1 @ (Just _) -> m1 -- if p1 succeeds, just return what it did
        Nothing -> p2 input

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
        Nothing -> Nothing                    -- p1 fails? we fail
        Just(terms1, remain1) ->
            case p2 remain1 of          -- run p2 on remaining input
                Nothing -> Nothing            -- p2 fails? we fail
                Just(terms2, remain2) ->   -- both succeeded
                    Just(terms1 ++ terms2, remain2)


------------------------------------------------------------

    ---------- (code for unification algorithm) ----------

unify equations = Nothing -- <-- stub code

----------------------------------------------------------------------

    ---------- (code for textual substitution) ----------

    --see bottom of file for code

substitute var1 term1 term2 = sub_apply [(var1, term1)] (term2)--sub_apply [("Y", ID "g")] (FCN "Y" [VAR "Y"])


----------------------------------------------------------------------
--
-- Some sample data
--
p1 = "{X=Y,X=3}"                    -- Solution "Y = 3, X = 3" (not "Y = 3, X = Y")
p2 = "{X=1,X=3}"                    -- Unification fails (tries to unify 1 and 3)
p3 = "{f(a,Y) = f(X,b), c = Z}"     -- Solution "Z = c, Y = b, X = a"
p4 = "{f(X) = g(Y)}"                -- Unification fails (different function names)
p5 = "{f(X,Y) = f(X)}"              -- unification fails (different # of arguments)
p6 = "{f(f(f(f(a, Z), Y), X), W) = f(W, f(X, f(Y, f(Z, a))))}"
-- p6 Solution is "Z = a, Y = f(a, a), X = f(f(a, a), f(a, a)),
--                 W = f(f(f(a, a), f(a, a)), f(f(a, a), f(a, a)))"
-- (found p6 in Wikipedia)


term string = 
    let 
        cleanString = filter (\x -> x /= ' ') string
        items = split string
        firstItem = items !! 0
        firstItemIsFunction = '(' `elem` firstItem
    in 
        if firstItemIsFunction == False && ("_FAILURE" `isInfixOf` (show (cleanTerms (term_VarOrID cleanString))) == False) then term_VarOrID cleanString
        else if ("_FAILURE" `isInfixOf` (show (cleanTerms (term_Func cleanString))) == False) then term_Func cleanString
        else Nothing


term_VarOrID s =
    let 
        items = split s
        firstItem = items !! 0
        lastItems = drop 1 items
    in 
        if isVariableName (firstItem) == True then Just (VAR firstItem, concat lastItems)
        else if isIdentifierName (firstItem) == True then Just (ID firstItem, concat lastItems)
        else Nothing


term_Func s =
    let
        firstFunc = (gabber s) !! 0
        remaining = sublist (length firstFunc) (length s) s 

        firstParenLoc = justToInt (elemIndex '(' firstFunc)
        lastParenLoc = (length firstFunc) - justToInt (elemIndex ')' (reverse firstFunc)) - 1

        functionName = sublist 0 firstParenLoc firstFunc
        args = sublist firstParenLoc (lastParenLoc+1) firstFunc
        cleanArgs = sublist 1 (length args - 1) args
    in
        if balance s == False then Nothing
        else Just (FCN functionName (map cleanTerms (map term (split cleanArgs))), (drop 1 remaining)) 
        


balance xs = head cumulated == 0 && all (>= 0) cumulated where
  cumulated = scanr (+) 0 $ map depth xs
  depth '(' = -1 
  depth ')' = 1
  depth _ = 0


---UTILITY FUNCTIONS



isVariableName a = 
    let
        decider = False `elem` (map isUpper a) 
    in 
        if decider == True then False
        else True

isIdentifierName a =
    let
        decider = False `elem` (map isLower a) 
    in 
        if decider == True then False
        else True

isConstant a =
    let
        decider = False `elem` (map isDigit a) 
    in 
        if decider == True then False
        else True





prettyVar (VAR a) = a
prettyVar _ = "_FAILURE"

prettyID (ID a) = a
prettyID _ = "_FAILURE"

prettyConst (CONST a) = a
prettyConst _ = (-999999)


justToInt (Just a) = a
justToInt _ = (-1)



--sublist (substring)
sublist start end list = take (end - start) (drop start list)
sublist2 list start end = take (end - start) (drop start list)

--split at commas
split = _split []
_split ts "" = ts
_split ts s = _split (ts ++ [(token s)]) (drop (length(token s) + 1) s)

token = _token ""
_token ys "" = ys
_token ys (x:xs) = 
    do
        if x == ',' then ys
        else _token (ys ++ [x]) xs


--
locator [] _ count out = out
locator (x:xs) i count out
    | i == x = (locator xs i (count+1) (out ++ [count]))
    | otherwise = (locator xs i (count+1) out)

--grabs function from list of arguments if its in pos 0 in string s
gabber s =
    let
        listOfCloseLoc = locator s ')' 1 [] -- locates closeing paren                       : locator "f(a,B,g(X)),r(x,y)" ')' 1 [] --> [10,11,18]
        fSublister = sublist2 s 0  -- sublist function waiting for end points               : func = sublist2 "f(a,B,g(X)),r(x,y)" 0
        possible = map fSublister listOfCloseLoc -- gives list of possibly stopage points   : map func [10,11,18]  --> ["f(a,B,g(X)","f(a,B,g(X))","f(a,B,g(X)),r(x,y)"]
        filteredPossible = filter balance possible -- filters out the unbalanced possibles  : filter balance ["f(a,B,g(X)","f(a,B,g(X))","f(a,B,g(X)),r(x,y)"]  --> ["f(a,B,g(X))","f(a,B,g(X)),r(x,y)"]
        function = take 1 filteredPossible -- takes first occurence of balanced argument    : "f(a,B,g(X))"
    in
        function


--Just (Term, [Char])

cleanTerms (Just (term, _)) = term
cleanTerms Nothing = VAR "_FAILURE"










--------------SUBSTITUTION-----------
--pulled from homework 6

sub_apply (subt) term =
    let 
        isvariable = isVar term
        remaining = take (length subt - 1) subt
        action = take 1 (reverse subt)
        isfunction = isFcn term 

    in
        if isfunction == True && allArgsNotFunctions (getFcnArgs term) then FCN (getFcnID term) (map (applySingleTerm (action!!0) ) (getFcnArgs term))
        else if isfunction == True then FCN (getFcnID term) (sub_apply_helper subt (getFcnArgs term))
        else if isvariable && (length remaining == 0) then applySingleTerm (action!!0) term
        else if isvariable then sub_apply remaining (applySingleTerm (action!!0) term)
        else term
-- when term is a list of args
sub_apply_helper (subt) terms =
    let 
        modifiedlist = map (sub_apply (subt)) terms 
    in
        modifiedlist

isVar (VAR _) = True
isVar _ = False

isFcn (FCN _ _) = True
isFcn _ = False

getFcnArgs (FCN _ args) = args
getFcnID (FCN id _) = id

applySingleTerm (var, term) expr =
    let 
        title = getTitle expr
    in
        if title == Nothing then expr
        else if title == Just var then term
        else expr

allArgsNotFunctions args =
    let
        maped = map (isFcn) args
    in
        if True `elem` maped then False
        else True



getTitle (VAR a) = Just a
getTitle (ID _) = Nothing
getTitle (FCN _ _) = Nothing







