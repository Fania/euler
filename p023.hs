import Data.List
import Control.Parallel (par, pseq)

factors n = [x | x <- [1..n], n `mod` x == 0]
divisors n = init $ factors n

isAbundant n = (sum $ divisors n) > n
abundants = [x | x <- [1..], isAbundant x]
firstAbunds = take 14062 abundants
sumAbunds = [ x + y | x <- abundants, y <- abundants]

sumOf n = [ (x,y) | x <- [1..n], y <- [1..(n-x)], (x + y) == n ]

halfSum pairs = take ((length pairs) `div` 2) pairs

test n = [ (x,y) | (x,y) <- sumOf n, (isAbundant x) && (isAbundant y) ]

listNums xs = rmdups $ concat [ [x,y] | (x,y) <- test xs ]

rmdups = map head . group . sort

-- main = print $ sum [ x | x <- [24..28123], (listNums x) == [] ]

stuff 24 = 0
stuff x
  | (listNums x) == [] = x + (stuff (x-1))
  | otherwise = stuff (x-1)

-- stuffB 14062 = 0
-- stuffB x
--   | (listNums x) == [] = x + (stuffB (x-1))
--   | otherwise = stuffB (x-1)

-- stuff1 24 = []
-- stuff1 x
--   | (listNums x) == [] = x : (stuff1 (x-1))
--   | otherwise = stuff1 (x-1)

main = print $ stuff 28123
