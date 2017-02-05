module Misc where

-- Problem 3

primeFactors' 0 _ = []
primeFactors' x i
  | (current * current > x) = [x]
  | x `mod` current == 0 = current : primeFactors' (x `div` current) 0
  | otherwise = primeFactors' x (i + 1)
    where current = primes !! i

primes = filterPrime [2..] 
  where filterPrime (p:xs) = 
          p : filterPrime [x | x <- xs, x `mod` p /= 0]