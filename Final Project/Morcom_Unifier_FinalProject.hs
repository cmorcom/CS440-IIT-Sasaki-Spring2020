-- Christopher Morcom (A20385764)
-- CS 440 - Final Project
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

--instanceOf(Type, a) methods
isVar :: a -> Bool
isVar (VAR _) = True
isVar _ = False

isID:: a -> Bool
isID (ID _) = True
isID _ = False

isConst:: a -> Bool
isConst (CONST _) = True
isConst _ = False

isFcn:: a -> Bool
isFcn (FN _ _) = True
isFcn _ = False

--pprint methods for each type
removeJust(Just x) = x

--drop whitespace methods
dropWhiteSpace x = [c | c <- x, not(c `elem` " \t\n") ]
dropSpaces x = dropWhile isSpace x
