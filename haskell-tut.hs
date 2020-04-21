--See Tutorial on: https://www.youtube.com/watch?v=02_H3LjqMr8

import Data.List
import System.IO

--Haskell uses type inference
--statically typed
-- Int (long)
maxint = maxBound :: Int
minInt = minBound :: Int
-- Integer (unbounded whole number) (most common) 
--Float and Double
bigFloat = 3.99999999999999+0.000000000005
--Bool type: True or False
--Char Type
-- Tuple

--CONSTANTS
always5 :: Int
always5 = 5



--FUNCTIONS
sumNums = sum [1..1000] --list generator
--can do basic math (/,*,+,-)

--prefix operator (prefix to args)
modex1 = mod 5 2 --(normal prefix mod)
modex2 = 5 `mod` 2 --(infix operator mod)

-- note the associativity
-- a = 5 + -4 versus
a = 5 + (-4)

--Here are some builtin functions
pie = pi
ePow9 = exp 9
logOf9 = log 9
squareOf3 = 3 ** 2
truncVal = truncate 3.999
rounding9 = round 8.5
ceil10 = ceiling 9.1
floor9 = floor 9.99
--also inclure all trig functions (sin, asin, sinh, etc)


--TYPES
-- note the output of ":t sqrt" --> "sqrt :: Floating a => a -> a"
num9 = 9 :: Int --define an INTEGER num9 = 9
sqrtOf9 = sqrt(fromIntegral num9)
-- ^^^ note that we have to cast this to make it work with sqrt ^^^


-- AND and OR
tf1 = True && False
tf2 = True || False
notTrue = not(True)


--LISTS basics (operators)
primes = [3,5,7,11]
moreprimes = primes ++ [1,13]
somenums = 2:3:4:[] --construction 
listinlist = [[1],[2]]
prepending = 2:moreprimes
primesLength = length primes
reverselist = reverse primes
isEmpty = null primes

accessvalue = primes !! 2
firstval = head primes
lastval = tail primes
allbutlastvalue = init primes
firstthreevals = take 3 primes
removeThree = drop 3 primes --gets all but first n values

exists = 3 `elem` primes--check if val in list
getmax = maximum primes
getmin = minimum primes

--get product of all vals in list
prodPrimes = product primes
--generate list
genlist = [9..30]
evensOnly = [2,4..20]
charlist = ['a','c'..'z']
infinPow10 = [10,20..] --infinite list (calc until nth item on runtime)
repeat2s = take 10 (repeat 2) --get first 10 elements
replicate3s = replicate 10 3 --repeat 3 ten times 
listcycle = take 10 (cycle [1,0,2,9])
--mult all vals by 2
listTimes3 = [x * 3 | x <- primes] --pull all values from list into x, sequentially and multiply it by 3 and put it in new list

filteredList = [x*3|x<-primes, x*3 <= 30] --filtering list given by condition specified after comma
example2 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0] --to add another filter, add another comma (get all vals divisible by 9 and 13)

sortedprimes = sort [11,9,7,5,3,2,1] --sort list

negList = [(-5)..(-1)] 
sumOfLists = zipWith (+) negList (reverse [1..5]) --sum values between two lists and return new list

listbiggerthan5 = filter (>5) primes --return list matching condition
evensupto20 = takewhile (<=20) [1..1000]

x=1
multOfList = foldr (*) x [2,3,4,5] --multiply everything in list by x FROM RIGHT TO LEFT
multOfList = foldl (*) x [2,3,4,5] --multiply everything in list by x FROM LEFT TO RIGHT

--video time: (25:29 of 1:16:47)