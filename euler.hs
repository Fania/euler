import Misc -- used by problems: 3

-- 1: Multiples of 3 and 5
problem1 = sum [x | x <- [1..999], (mod x 3) == 0 || (mod x 5) == 0]

-- 2: Even Fibonacci numbers
problem2 = sum [x | x <- takeWhile (< 4000000) (fib 0 1)]
  where fib x y | even (x + y) = [x + y] ++ fib y (x+y)
                | otherwise = fib y (x+y)

-- 3: Largest prime factor
problem3 = maximum (primeFactors' 600851475143 0)

-- 4: Largest palindrome product
problem4 = maximum [x * y | x <- [100..999], y <- [100..999], isPal (x * y)]
  where isPal x = show x == reverse(show x)

-- 5: Smallest multiple
{-
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-}

problem5 = head [ x | x <- [1..], check1 x]
  where divBy x y = mod x y == 0
        check1 x = all (==True) (map (divBy x) [1..10])

divBy x y = mod x y == 0


check x [] = True
check x (y:ys) 
  | divBy x y = check x ys
  | otherwise = False
test = [ x | x <- [1..], check x [1..10] ]


problem5a = head [ x | x <- [1..], check1 x == False]
  where divBy x y = mod x y == 0
        check1 x = any (==False) (map (divBy x) [1..20])