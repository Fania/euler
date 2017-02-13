import Data.List
import Data.Numbers.Primes

-- 32: Pandigital products

isPandigital n = all (==True) $ ((map (`elem` [1..l]) xs) ++ [unique xs])
  where l = length $ intToList n
        xs = intToList n

unique [n] = True
unique (n:ns) 
  | n `notElem` ns = unique ns
  | otherwise = False

uniqDigits ns = unique $ intToList ns

factors n = [x | x <- [1..n], n `mod` x == 0]

panDfactors x = [ (a,b) | a <- (filter (uniqDigits) (factors x)), 
                          b <- (filter (uniqDigits) (factors x)), 
                          a*b == x, 
                          not (contains0 a), not (contains0 b),
                          unique (eqToList a b x) ]

panDfacts x = take n (panDfactors x)
  where n = length (panDfactors x) `div` 2

contains0 n = 0 `elem` (intToList n)
-- fact x = [ (a,b) | a <- (factors x), b <- (factors x), a*b == x ] 

eqToList a b c = concat $ map intToList [a,b,c]

check2 a b c = isPandigital $ listInt $ listNum $ eqToList a b c

-- problem32 = [ (a,b,c) | c <- [1..], (a,b) <- (panDfactors c), 
--                          (length (eqToList a b c)) == 9,
--                          check2 a b c,
--                          a*b == c]

intToList n = map ( \x -> read [x] :: Int ) ( show n )

numList n = map ( \x -> read [x] :: Int ) ( show n )

listNum [] = []
listNum (n:ns) = (show n) ++ listNum ns

listInt s = read s :: Int

numsWithout0 = [ x | x <- [1..], 0 `notElem` (intToList x) ]

numsWithPanFacts = nub [ c | c <- numsWithout0, (a,b) <- panDfacts c ]

main = print $ [ (a,b,c) | c <- (take 9999 numsWithPanFacts), 
                         (a,b) <- (panDfacts c), 
                         (length (eqToList a b c)) == 9]