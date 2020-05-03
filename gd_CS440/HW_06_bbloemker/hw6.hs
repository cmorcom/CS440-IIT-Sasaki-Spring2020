import Data.List

type Variable = String
type Identifier = String
data Term = VAR Variable | ID Identifier | FCN Identifier [Term] 
    deriving (Show, Eq, Read)
type Substitution = [(Variable, Term)]


normalizeJust (Just a) = a

sub_lookup subt var = 
    let listOfBinding = filter (\(x,y)-> x==var) subt
    in
        if length listOfBinding == 0 then Nothing
        else Just listOfBinding

sub_update subt (var, term) =
    let bindingToReplace = sub_lookup subt var
    in
        if bindingToReplace == Nothing then subt ++ [(var, term)]
        else map (change (var, term)) subt


change (x,y) (item,expr)
    | x == item = (x,y)
    | otherwise = (item,expr)

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






locator [] _ _ = -1
locator (x:lst) item n = 
    if x == item then n
    else locator lst item (n+1)
