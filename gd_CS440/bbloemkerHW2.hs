-- BJ Bloemker
-- A20372354


--Question 1

-- 
-- a. f1a :: (b, a) -> (a, b)
-- b. f1b :: a -> [a] -> [[a]]
-- c. f1c :: a -> a -> [a] -> [[a]]
-- d. f1d :: (a -> Bool) -> [a] -> Int
-- 

-- Question 2
iterate1 :: (Ord t, Num t) => t -> (c -> c) -> c -> c
iterate1 n f | n <= 0 = (id)
iterate1 1 f = (f)
iterate1 n f = (f . (iterate1 (n-1) f))

-- Question 3
iterate2 :: Int -> (a -> a) -> a -> a
iterate2 n f | n <= 0 = (id)
iterate2 n f = foldr (.) id (take n (repeat f))

-- Question 4
data Tree a = Leaf a | Branch a (Tree a) (Tree a)

treeEq :: Eq a => Tree a -> Tree a -> Bool
treeEq (Leaf x) (Leaf y) = x==y
treeEq (Branch a l1 l2) (Branch b r1 r2) = (a ==b) && treeEq l1 r1 && treeEq l2 r2
treeEq _ _ = False

--Quesion 5
showTree :: Show a => Tree a -> [Char]
showTree (Leaf l) = "Leaf " ++ show l
showTree (Branch a l1 r2) = "Branch " ++ show a ++ " (" ++showTree l1 ++ ") " ++ "(" ++showTree r2 ++")"

-- Question 6
preorder :: Tree a -> [a]
preorder (Leaf a ) = [a]
preorder (Branch a t1 t2 ) = a : (preorder t1 ++ preorder t2)

-- Question 7
preorder' :: Tree a -> [a] -> [a]
preorder' a x = preorder a ++ x

preorder2 :: Tree a -> [a]
preorder2 t = preorder' t []

-- Question 8
data Pattern a = P a | POr (Pattern a) (Pattern a) | PAnd (Pattern a) (Pattern a) deriving Show

match :: Eq a => Pattern a -> [a] -> (Bool, [a])
match _ [] = (False, [])
match (P x) (y : ys) = if x == y then (True, ys) else (False, y : ys)
match (POr pat1 pat2) xs = case match pat1 xs of
    (True, leftover) -> (True, leftover)
    -- stop if pat1 succeeded
    (False, _) -> match pat2 xs
    -- else try pat2
match (PAnd pat1 pat2) xs = case match pat1 xs of
    (True, leftover) -> case match pat2 leftover of
        (True, leftover)-> (True, leftover)
        (False, _) -> (False, xs)
    (False, _)-> (False, xs)
