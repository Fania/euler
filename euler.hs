import Data.Char -- problems: 8
import Data.List -- problems: 11


-- 1: Multiples of 3 and 5
problem1 = sum [x | x <- [1..999], (mod x 3) == 0 || (mod x 5) == 0]
-- 233168
--------------------------------------------------------------------------


-- 2: Even Fibonacci numbers
problem2 = sum [x | x <- takeWhile (< 4000000) (fib 0 1)]
  where fib x y | even (x + y) = [x + y] ++ fib y (x+y)
                | otherwise = fib y (x+y)
-- 4613732
--------------------------------------------------------------------------


-- 3: Largest prime factor
primes = filterPrime [2..] 
  where filterPrime (p:xs) = 
          p : filterPrime [x | x <- xs, x `mod` p /= 0]

primeFactors' 0 _ = []
primeFactors' x i
  | (current * current > x) = [x]
  | x `mod` current == 0 = current : primeFactors' (x `div` current) 0
  | otherwise = primeFactors' x (i + 1)
    where current = primes !! i

problem3 = maximum (primeFactors' 600851475143 0)
-- 6857
--------------------------------------------------------------------------


-- 4: Largest palindrome product
problem4 = maximum [x * y | x <- [100..999], y <- [100..999], isPal (x * y)]
  where isPal x = show x == reverse(show x)
-- 906609
--------------------------------------------------------------------------


-- 5: Smallest multiple
-- UNSOLVED
problem5 = head [ x | x <- [1..], check1 x]
  where divBy x y = mod x y == 0
        check1 x = all (==True) (map (divBy x) [1..10])

-- divBy x y = mod x y == 0

-- check x [] = True
-- check x (y:ys) 
--   | divBy x y = check x ys
--   | otherwise = False
-- test = [ x | x <- [1..], check x [1..10] ]

-- problem5a = head [ x | x <- [1..], check1 x == False]
--   where divBy x y = mod x y == 0
--         check1 x = any (==False) (map (divBy x) [1..20])
--------------------------------------------------------------------------


-- 6: Sum square difference
problem6 = sqsm - smsq
  where smsq = sum (map (^2) [1..100])
        sqsm = (sum [1..100]) ^2
-- 25164150
--------------------------------------------------------------------------


-- 7: 10001st prime
problem7 = primes !! 10000
-- 104743
--------------------------------------------------------------------------


-- 8: Largest product in a series
problem8 = maximum (map prod13 (seq13wo0 (seq13 num)))
  where seq13 [a,b,c,d,e,f,g,h,i,j,k,l,m] = [[a,b,c,d,e,f,g,h,i,j,k,l,m]]
        seq13 ns = [take 13 ns] ++ seq13 (tail ns)
        prod13 [] = 1
        prod13 (x:xs) = (digitToInt x) * prod13 xs
        seq13wo0 xss = [xs | xs <- xss,  (contains0 xs) == False]
        contains0 xs = '0' `elem` xs
        num = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
-- 23514624000
--------------------------------------------------------------------------


-- 9: Special Pythagorean triplet
problem9 = head [a*b*(1000-(a+b)) | a <- [1..998], b <- [1..(999-a)], 
                 isPyth a b (1000-(a+b)), (a+b+(1000-(a+b)))==1000]
  where isPyth x y z = x^2 + y^2 == z^2
-- 31875000
--------------------------------------------------------------------------


-- 10: Summation of primes
-- UNSOLVED
problem10 = sum (take 2000000 primes)
--------------------------------------------------------------------------


-- 11: Largest product in a grid
grid = [[08,02,22,97,38,15,00,40,00,75,04,05,07,78,52,12,50,77,91,08],
        [49,49,99,40,17,81,18,57,60,87,17,40,98,43,69,48,04,56,62,00],
        [81,49,31,73,55,79,14,29,93,71,40,67,53,88,30,03,49,13,36,65],
        [52,70,95,23,04,60,11,42,69,24,68,56,01,32,56,71,37,02,36,91],
        [22,31,16,71,51,67,63,89,41,92,36,54,22,40,40,28,66,33,13,80],
        [24,47,32,60,99,03,45,02,44,75,33,53,78,36,84,20,35,17,12,50],
        [32,98,81,28,64,23,67,10,26,38,40,67,59,54,70,66,18,38,64,70],
        [67,26,20,68,02,62,12,20,95,63,94,39,63,08,40,91,66,49,94,21],
        [24,55,58,05,66,73,99,26,97,17,78,78,96,83,14,88,34,89,63,72],
        [21,36,23,09,75,00,76,44,20,45,35,14,00,61,33,97,34,31,33,95],
        [78,17,53,28,22,75,31,67,15,94,03,80,04,62,16,14,09,53,56,92],
        [16,39,05,42,96,35,31,47,55,58,88,24,00,17,54,24,36,29,85,57],
        [86,56,00,48,35,71,89,07,05,44,44,37,44,60,21,58,51,54,17,58],
        [19,80,81,68,05,94,47,69,28,73,92,13,86,52,17,77,04,89,55,40],
        [04,52,08,83,97,35,99,16,07,97,57,32,16,26,26,79,33,27,98,66],
        [88,36,68,87,57,62,20,72,03,46,33,67,46,55,12,32,63,93,53,69],
        [04,42,16,73,38,25,39,11,24,94,72,18,08,46,29,32,40,62,76,36],
        [20,69,36,41,72,30,23,88,34,62,99,69,82,67,59,85,74,04,36,16],
        [20,73,35,29,78,31,90,01,74,31,49,71,48,86,81,16,23,57,05,54],
        [01,70,54,71,83,51,54,69,16,92,33,48,61,43,52,01,89,19,67,48]]

diag4lr x y = [(grid !!  x)    !!  y,    (grid !! (x+1)) !! (y+1), 
               (grid !! (x+2)) !! (y+2), (grid !! (x+3)) !! (y+3) ]

diag4rl x y = [(grid !!  x)    !!  y,    (grid !! (x+1)) !! (y-1), 
               (grid !! (x+2)) !! (y-2), (grid !! (x+3)) !! (y-3) ]

setsOf4Dlr = [ diag4lr x y | x <- [0..16], y <- [0..16] ]
setsOf4Drl = [ diag4rl x y | x <- [0..16], y <- [3..19] ]

setsOf4 [w,x,y,z] = [[w,x,y,z]]
setsOf4 xs = take 4 xs : setsOf4 ( tail xs )

maxHgrid = concat [ setsOf4 xs | xs <- grid ]
maxVgrid = concat [ setsOf4 xs | xs <- (transpose grid) ]
setsOf4D = concat [setsOf4Dlr,setsOf4Drl]

prod4 [w,x,y,z] = w*x*y*z
maxHprod = maximum (map prod4 maxHgrid)
maxVprod = maximum (map prod4 maxVgrid)
max4Dprod = maximum (map prod4 setsOf4D)

problem11 = maximum $ concat [[maxHprod], [maxVprod], [max4Dprod]]
-- 70600674
--------------------------------------------------------------------------


-- 12: Highly divisible triangular number
-- UNSOLVED
tnum 0 = 0
tnum x = x + tnum (x-1)
tnums = [ tnum x | x <- [1..] ]

factors n = [x | x <- [1..n], n `mod` x == 0]

problem12 = head [ x | x <- tnums, length (factors x) > 100 ]
--------------------------------------------------------------------------


-- 14: Longest Collatz sequence
-- UNSOLVED
collatz x | even x    = x `div` 2
          | otherwise = 3 * x + 1

collseq 1 = [1]
collseq n = n : collseq (collatz n)

problem14 = snd $ maximum $ [ (length (collseq x), x) | x <- [1..999999] ]
--------------------------------------------------------------------------


-- 15: Lattice paths
-- UNSOLVED
--------------------------------------------------------------------------


-- 16: Power digit sum
problem16 = sum $ map ( \x -> read [x] :: Int ) ( show $ 2 ^ 1000 )
-- 1366
--------------------------------------------------------------------------


-- 17: Number letter counts
singles = [(1,"one"), (2,"two"), (3,"three"), (4,"four"), (5,"five"),
           (6,"six"), (7,"seven"), (8,"eight"), (9,"nine"), (0,"")]

irregulars = [(0,"ten"),(1,"eleven"),(2,"twelve"),(3,"thirteen"),
              (4,"fourteen"),(5,"fifteen"),(6,"sixteen"),(7,"seventeen"),
              (8,"eighteen"),(9,"nineteen")]

tens = [(2,"twenty"),(3,"thirty"),(4,"forty"),(5,"fifty"),
        (6,"sixty"),(7,"seventy"),(8,"eighty"),(9,"ninety")]

hundreds n = unjust (lookup n singles) ++ "hundred"
thousands n = unjust (lookup n singles) ++ "thousand"

numList n = map ( \x -> read [x] :: Int ) ( show n )

listNum [] = []
listNum (n:ns) = (show n) ++ listNum ns

listInt s = read s :: Int

translate [x] = unjust (lookup x singles)
translate [x,y] 
  | x == 0 = translate [y]
  | x == 1 = unjust (lookup y irregulars)
  | otherwise = unjust (lookup x tens) ++ unjust (lookup y singles)
translate [x,y,z]
  | x == 0 = translate [y,z]
  | x `elem` [1..9] && y == 0 && z `elem` [1..9] = hundreds x ++ "and" ++ translate [z]
  | x `elem` [1..9] && y == 0 && z == 0 = hundreds x
  | otherwise = hundreds x ++ "and" ++ translate [y,z]
translate [w,x,y,z] = thousands w ++ translate [x,y,z]

unjust Nothing = []
unjust (Just x) = x

problem17 = length ( concat [ translate (numList x) | x <- [1..1000]] )
-- 21124
--------------------------------------------------------------------------


-- 18: Maximum path sum I
-- UNSOLVED
pyramid =     [[75],
              [95,64],
             [17,47,82],
            [18,35,87,10],
           [20,04,82,47,65],
          [19,01,23,75,03,34],
         [88,02,77,73,07,63,67],
        [99,65,04,28,06,16,70,92],
       [41,41,26,56,83,40,80,70,33],
      [41,48,72,33,47,32,37,16,94,29],
     [53,71,44,65,25,43,91,52,97,51,14],
    [70,11,33,28,77,73,17,78,39,68,17,57],
   [91,71,52,38,17,14,91,43,58,50,27,29,48],
  [63,66,04,68,89,53,67,30,73,16,69,87,40,31],
 [04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]]

baby = [[3],
       [7,4],
      [2,4,6],
     [8,5,9,3]]


-- createTree (x:xs) = Fork (createTree xs) (head x) (createTree xs)


babytree = Fork (Fork (Fork (Leaf 8) 2 (Leaf 5)) 7 (Fork (Leaf 5) 4 (Leaf 9))) 3 (Fork (Fork (Leaf 5) 4 (Leaf 9)) 4 (Fork (Leaf 9) 6 (Leaf 3)))

data Tree = Leaf Int | Fork Tree Int Tree 
  deriving (Show)

travel (Leaf x) = [[x]]
travel (Fork l x r) = [x : s | t <- [l,r], s <- travel t]

problem18 = maximum [ sum path | path <- travel babytree ]
--------------------------------------------------------------------------


-- 19: Counting Sundays
-- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

-- 1 Jan 1900 was a Monday.
-- Thirty days has September,
-- April, June and November.
-- All the rest have thirty-one,
-- Saving February alone,
-- Which has twenty-eight, rain or shine.
-- And on leap years, twenty-nine.
-- A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.


-- getday d m y = 
--------------------------------------------------------------------------


-- 20: Factorial digit sum
problem20 = sum $map ( \x -> read [x] :: Int ) ( show (fact 100) )
  where fact n = product [1..n]
-- 648
--------------------------------------------------------------------------


-- 21: Amicable numbers
-- SLOW
divisors n = init $ factors n

d n 
  | divisors n == [] = Nothing
  | otherwise = Just (sum $ divisors n)

unjuster Nothing = 0
unjuster (Just n) = n

amicable n 
  | d n == Nothing = False
  | unjuster (d n) == n = False
  | otherwise = n == unjuster (d (unjuster (d n)))

amis n = [ x | x <- [1..n], amicable x ]

problem21 = sum (amis 9999)
-- 31626
--------------------------------------------------------------------------

