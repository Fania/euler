-- Multiples of 3 and 5
problem1 = sum [x | x <- [1..999], (mod x 3) == 0 || (mod x 5) == 0]

-- Even Fibonacci numbers
problem2 = sum [x | x <- takeWhile (< 4000000) (fib 0 1)]
            where fib x y | even (x + y) = [x + y] ++ fib y (x+y)
                          | otherwise = fib y (x+y)

-- Largest prime factor
problem3 = 

primesTo m = sieve [2..m] {- (\\) is set-difference for unordered lists -}
             where 
             sieve (x:xs) = x : sieve (xs Data.List.\\ [x,x+x..m])
             sieve [] = []