-- Christopher Morcom (A20385764)
-- CS 440 Final Project
{- ARCHIVE
    --Note: problem in format: "{ eqn , **eqns }" (whitespace doesn't matter)
    parse_problem :: String -> [String]
    parse_problem [] = []
    -- break into equations and parse here

    --Split a single equation into two terms
    parse_equation :: String -> [String]
    parse_equation [] = []
    parse_equation input = match_terms(
        let tmp = break (== '=') (dropInnerSpaces input)
        in [fst(tmp), tail(snd(tmp))])

    match_terms :: [String] -> [String]
    match_terms [] = []
    match_terms input = [input!!0, input!!1]
-}

-- Spring 2020 
module Morcom_Unifier_FinalProject where
import Data.Char
import Text.Printf
import Data.List

-- basic symbols/Constants and parser type (From HW6 Solution)
type Parser t = String -> Maybe(t, String)

type Symbol = Char
type Constant = Char
comma  = ',' :: Symbol 
lparen = '(' :: Symbol
minus  = '-' :: Symbol
plus   = '+' :: Symbol
rparen = ')' :: Symbol
star   = '*' :: Symbol 
equals = '=' :: Symbol
lbrace = '{' :: Symbol
rbrace = '}' :: Symbol

{- Grammar for unification problems (new rules for problems, equations, and modified rule for factors):
        Problem → { Equations }
        Equation → Expr = Expr
        Equations → Equation | Equation , Equations
        Expr → … as before, with +, * ...
        Factor → identifier | variable
        Factor → constant | \( Expr \)|Function_call
        Function_call → identifier \( (Arguments|ε) \) // includes calls like f()
        Arguments → Term | Term , Arguments
-}
type Problem = [Equation]
type Equation = (Term, Term)
type Subst = (Variable, Term)
type Variable = String
type Identifier = String

data Term = 
    Empty
    | VAR Variable 
    | ID Identifier 
    | CONST Int 
    | FN Identifier [Term] 
    deriving (Show, Eq, Read)

-------------------------------------------------------------------------------------------------------------------
-- TEST CASES --
p1 = "{X=Y,X=3}"                    
p2 = "{X=1,X=3}"                   
p3 = "{f(a,Y) = f(X,b), c = Z}"     
p4 = "{f(X) = g(Y)}"                
p5 = "{f(X,Y) = f(X)}"             
p6 = "{f(f(f(f(a, Z), Y), X), W) = f(W, f(X, f(Y, f(Z, a))))}"

-------------------------------------------------------------------------------------------------------------------

-- Pattern match using cases to solve the problem --

solve :: String -> String
solve a = case problem a of
    Nothing -> "FAIL: Nothing to Unify"
    Just (eqns, _) -> case unifyProblem eqns of
        Nothing -> "FAIL: Cannot Unify"
        Just subs ->
            do (substr 0 (length noiceString - 2) noiceString) ++ "}" 
            where noiceString = prettyString (stripJust (unifyProblem eqns)) []

parse :: String -> Maybe[Subst]
parse str = case problem str of 
    Nothing -> Nothing
    Just(eqns, _) -> unifyProblem eqns

-- TODO: pprint :: String -> string

prettyString [] str = concat (["{"] ++ [str])
prettyString subs str = prettyString (tail subs) ((fst(subs!!0))++"="++(strip(snd(subs!!0)))++", "++str)

-------------------------------------------------------------------------------------------------------------------

-- Lexical Analysis --
{- 
identifiers and variables are a letter followed by alphanumerics or underscore.
    • IDs start with lowerCASE (ex. aB_1)
    • VARs start with UPPERCASE (ex. cD_2)
    • Whitespace can include \n.
    • constants are integers (ex. -1 or 2)
-}
digits = "0123456789"
letters_ = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
alphabet = init letters_

-- recursion level
lvl c 
    | c == lparen = -1 
    | c == rparen = 1
    | otherwise = 0


isValidName t = --we assume T starts with a letter before calling this
    if False `elem` [False | c <- t, c `notElem` (letters_++digits)] then False
    else True

startsWithAlpha "" = False
startsWithAlpha a = 
    do 
        if (c `elem` alphabet) then True 
        else False
    where c = head a 

isID_Name "" = False 
isID_Name t =
    if startsWithAlpha t then
        if (isLower (head t)) && (isValidName t) then True
        else False
    else False

isVar_Name "" = False
isVar_Name t =
    if startsWithAlpha t then
        if (isUpper (head t)) && (isValidName t) then True
        else False
    else False


isConst_Num "" = False
isConst_Num x = 
    do 
        if (head x) == minus then checkNum(tail x)
        else checkNum x
    where
        checkNum a = 
            if False `elem` (map isDigit a) then False else True

--check if parentheses are balanced (stack)
balanceParens a = (head x) == 0 && all (>=0) x where
    x = (scanr (+) 0 (map lvl a))

--getting substrings
substr s e l = take (e-s) (drop s l)

-- splitting on commas (recursively take chars until a comma appears then push the term to list until all chars consumed)
splitC = do c_split [] where
    c_split a "" = a
    c_split a b = c_split (a++[(tok b)]) (drop (length(tok b)+1) b) where
        tok = do c_tok "" where
            c_tok b "" = b
            c_tok b (x:xs) = if (x == comma) then b else c_tok (b++[x]) xs
                
-- splitting on equals (same idea as above)
splitE = do e_split [] where
    e_split a "" = a
    e_split a b = e_split (a++[(tok b)]) (drop (length(tok b)+1) b) where
        tok = do e_tok "" where
            e_tok b "" = b
            e_tok b (x:xs) = if (x == equals) then b else e_tok (b++[x]) xs

-- get something by via idx (ex. array[0])
get [] i c output = output
get (x:xs) i c output = if x == i then get xs i (c+1) (output ++[c]) else get xs i (c+1) output

--get Function from args 
getFnFromArgc str = do take 1 possibilities
    where 
        possibilities = do filter balanceParens ( map (_substr str 0) (get str ')' 1 []) ) where
            _substr l s e = take (e-s) (drop s l)


-------------------------------------------------------------------------------------------------------------------

-- Basic GET/Flag Methods (also strip types and pprint)--

isVar (VAR _) = True
isVar _ = False
stripVar(VAR a) = a

isID (ID _) = True
isID _ = False
stripID(ID a) = a

isConst (CONST _) = True
isConst _ = False
stripConst(CONST a) = a
stripConst _ = -1 --error on numeric parse

isFcn (FN _ _) = True
isFcn _ = False
stringFcn (FN id args) = concat ([id] ++ (map strip args))

getName (VAR a) = Just a
getName (ID _) = Nothing
getName (FN _ _) = Nothing

getFnID(FN id args) = id
getFnArgs(FN id args) = args

-- Strip for other types and symbols not above
stripJust(Just a) = a
stripJustInt(Just a) = a
stripJustInt _ = -1 -- error on Just numeric


--Stripping for when we don't know the type
strip a 
    | (isVar a) = (stripVar a)
    | (isID a) = (stripID a)
    | (isConst a) = show (stripConst a)
    | (isFcn a) = (stringFcn a)
    | otherwise = "_FAIL_"



-- Use this to get left and right sides of an eqn
getLeft (a,_) = a
getRight (_,a) = a

-- Get something of VAR type given TERMS
getVars [] a = a
getVars a lst = 
    do [( (stripVar t1) :: Variable, t2) :: Subst] ++ (getVars eqns_Tail lst) 
    where
        eqn = head a
        eqns_Tail = tail a
        t1 = getLeft eqn
        t2 = getRight eqn

applyTerm (v,t) exp = let name = getName exp in if name == Just v then t else exp
applyTermList (st) terms = map (subst (st)) terms

--check if args are function calls
checkArgs a
    | (True `elem` (map (isFcn) a)) = False
    | otherwise = True 

--drop whitespace methods
dropWhiteSpace x = [c | c <- x, not(c `elem` " \t\n") ]
dropSpaces x = dropWhile isSpace x

-- Parse/Match Function Call with Args --
matchFnIDs f1 f2 = if getFnID f1 == getFnID f2 then True else False
matchNumArgs f1 f2 = if (length (getFnArgs f1) == length (getFnArgs f2)) then True else False

-- EXTRA CREDIT: Function to print reason for failure --
unificationFail msg  = do { print("ERROR "++msg); return Nothing} 



-----------------------------------------------------------------------------------------------------------

-- Substitution Functions (TEXT) --
subst v t1 t2 = sub_help [(v,t1)] (t2)
    where
        sub_help (subTerm) term = 
            if isVar term then 
                if length (init subTerm) == 0 then
                    applyTerm ((tail subTerm)!!0) term
                else 
                    sub_help (init subTerm) (applyTerm ((tail subTerm)!!0) term)
            
            else if isFcn term then
                if checkArgs (getFnArgs term) then
                    FN (getFnID term) (map (applyTerm ((tail subTerm)!!0)) (getFnArgs term))
                else
                    FN (getFnID term) (map (sub_help (subTerm)) (getFnArgs term))

            else term 

patsubst _ _ [] factors = factors
patsubst var term eqns factors = 
    let subleft = subst var term (getLeft (head eqns)); 
        subright = subst var term (getRight (head eqns))
    in [(subleft, subright)] ++ (patsubst var term (tail eqns) factors)

-----------------------------------------------------------------------------------------------------------

-- Unification Parsing --

-- helper functions
getRemaining (Just (_,a)) = a
getRemaining _ = ""

stripEqnJust (Just((a,b),_)) = (a,b)
stripEqnJustToList (Just (t,_)) = t
stripEqnJustToList Nothing = VAR "_FAIL_"

_join a b = intercalate a b 

stripLeadingComma "" = ""
stripLeadingComma str = if (head str) == comma then tail str else str

stripBraces str = do if head str == lbrace && last str == rbrace then drop 1 (init str) else str

-- parse str as VAR or ID or FN and then extract Problem Terms
term str = 
    do {
        --check if str starts with fn call
        if not ('(' `elem` t1) then
            if "_FAIL_" `isInfixOf` (show (stripEqnJustToList (term_VarID purestr))) then term_VarID purestr
            else Nothing
        else if "_FAIL_" `isInfixOf` (show (stripEqnJustToList (term_Fn purestr))) then term_VarID purestr
        else Nothing
    }
    where {
        purestr = filter (\x -> x /= ' ') str;
        t1 = (splitC(purestr))!!0
    }

term_VarID str = 
    do
        if isVar_Name (ts!!0) then Just(VAR (ts!!0), ("," `_join` (tail ts)))
        else if isID_Name (ts!!0) then Just(ID (ts!!0), ("," `_join` (tail ts)))
        else if isConst_Num (ts!!0) then Just((CONST (read (ts!!0) :: Int)), ("," `_join` (tail ts)))
        else Just(VAR "_FAIL_", "")
    where ts = splitC str
    

term_Fn str = 
    do
        if balanceParens str == False then Just(VAR "_FAIL_", "")
        else if pureArgs == "," then Just(VAR "_FAIL_", "")
        else Just(FN fn_name args, (tail (substr (length f1) (length str) str)))
    where
        f1 = (getFnFromArgc str) !! 0
        outerBrackets_idx = stripJustInt (elemIndex lparen (reverse f1))
        deepestBrackets_idx = (length f1) - stripJustInt(elemIndex rparen (reverse f1)) - 1
        fn_name = substr 0 outerBrackets_idx f1
        arguments = substr outerBrackets_idx (deepestBrackets_idx + 1) f1
        pureArgs = substr 1 (length arguments - 1) arguments
        args = (map stripEqnJustToList (map term (splitC pureArgs)))

problem input = 
    do {
        if "_FAIL_" `isInfixOf` (show output) then Nothing
        else Just (output,"")
    } 
    where { output = prob_helper input [] }

prob_helper "" output = output
prob_helper input output = 
    do (prob_helper (getRemaining (justTerms)) (output ++ [stripEqnJust justTerms]))
    where justTerms = getProblemTerms input

getProblemTerms str = 
    do 
        if lparen `elem` (terms!!0) then
            let
                left = (stripEqnJustToList (term_Fn (t1)), stripEqnJustToList (term_Fn (t2)))
                right = stripLeadingComma( substr ( length t1+length t2+1) (length purestr) purestr )
            in Just(left,right) 
        else
            let 
                left = (stripEqnJustToList (term (last (reverse e1_split))), stripEqnJustToList (term (last e1_split)))
                right = "," `_join` (tail terms)
            in Just(left,right) 
    where
        purestr = stripBraces(filter (\x->x/=' ') str);
        terms = splitC purestr;
        e1_split = splitE (terms !! 0);
        t1 = (getFnFromArgc purestr)!!0;
        t2 = (getFnFromArgc(substr (stripJustInt (elemIndex equals purestr)+1) (length purestr) purestr))!!0
        

-- Unification algorithm --
unifyFcn [] a = a
unifyFcn eqns factors = 
    do 
        -- check the no. args and function IDs of each call
        if isFcn(t1) then 
            if matchFnIDs t1 t2 && matchNumArgs t1 t2 then --recurse, unifying the args of the functions first
                unifyFcn ((zip (getFnArgs t1) (getFnArgs t2)) ++ eqns_Tail) factors 
            else 
                [] --function IDs or no. args are not the same

        -- This will ensure the function call depth of the left and right terms is equal
        else if (isConst t1 || isID t1) then 
            if t1 /= t2 then []
            else 
                unifyFcn eqns_Tail factors --arg is a function call. recurse and unify
        
        --Always have the variable on the left
        else if (not (isVar t1) && isVar t2) then 
            unifyFcn ([(t2,t1)]++eqns_Tail) factors 

        -- Case if terms already matched (skip case)
        else if t1 == t2 then unifyFcn eqns_Tail factors

        else
            unifyFcn (patsubst (stripVar t1) t2 eqns_Tail []) ([(t1,t2)]++patsubst (stripVar t1) t2 eqns_Tail [])

    where {
        eqns_Head = (eqns !! 0);
        eqns_Tail = drop 1 eqns;
        t1 = getLeft eqns_Head;
        t2 = getRight eqns_Head
    }

unifyProblem problem = 
    do 
        if fcall == [] then Nothing
        else Just factors
    where { fcall = unifyFcn problem []; factors = getVars fcall [] }