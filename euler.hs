import Numeric
import Data.Char
import Data.List
import Data.Tuple
import Data.Matrix
import Data.Ratio
import Data.Numbers.Primes

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
problem3 = maximum (primeFactors 600851475143)
-- 6857

--------------------------------------------------------------------------

-- 4: Largest palindrome product
isPal x = show x == reverse(show x)
problem4 = maximum [x * y | x <- [100..999], y <- [100..999], isPal (x * y)]
  -- where isPal x = show x == reverse(show x)
-- 906609

--------------------------------------------------------------------------

-- 5: Smallest multiple
-- UNSOLVED
problem5 = head [ x | x <- [1..], check1 x]
  where divBy x y = mod x y == 0
        check1 x = all (==True) (map (divBy x) [1..10])

-- divBy x y = mod x y == 0

{-
check x [] = True
check x (y:ys) 
  | divBy x y = check x ys
  | otherwise = False
test = [ x | x <- [1..], check x [1..10] ]

problem5a = head [ x | x <- [1..], check1 x == False]
  where divBy x y = mod x y == 0
        check1 x = any (==False) (map (divBy x) [1..20])
-}
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
isPyth x y z = x^2 + y^2 == z^2
problem9 = head [a*b*(1000-(a+b)) | a <- [1..998], b <- [1..(999-a)], 
                 isPyth a b (1000-(a+b)), (a+b+(1000-(a+b)))==1000]
  -- where isPyth x y z = x^2 + y^2 == z^2
-- 31875000

--------------------------------------------------------------------------

-- 10: Summation of primes
problem10 = sum (takeWhile (< 2000000) primes)
-- 142913828922

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
maxVgrid = concat [ setsOf4 xs | xs <- (Data.List.transpose grid) ]
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
months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]

monyear = [ (m,y) | y <- [1900 .. 2000], m <- months ]

m30s = filter (\(m,y) -> m `elem` ["Apr","Jun","Sep","Nov"]) monyear
m31s = filter (\(m,y) -> ((m,y) `notElem` m30s) && m /= "Feb") monyear
m29s = filter (\(m,y) -> m == "Feb" && (y `mod` 4) == 0 && y /= 1900) monyear
m28s = filter (\(m,y) -> m == "Feb" && ((m,y) `notElem` m29s)) monyear

addDays (m,y)
  | (m,y) `elem` m30s = 
    [ (d,m,y) | d <- [1..30] ]
  | (m,y) `elem` m29s = 
    [ (d,m,y) | d <- [1..29] ] -- Leap years
  | (m,y) `elem` m28s = 
    [ (d,m,y) | d <- [1..28] ] -- Non-leap years
  | otherwise = [ (d,m,y) | d <- [1..31] ]

allDates = concat $ map addDays monyear

everyf n [] = []
everyf n as  = head as : everyf n (drop n as)

mondays =    [ ("Mon",d,m,y) | (d,m,y) <- (everyf 7 allDates) ]
tuesdays =   [ ("Tue",d,m,y) | (d,m,y) <- (everyf 7 (drop 1 allDates)) ]
wednesdays = [ ("Wed",d,m,y) | (d,m,y) <- (everyf 7 (drop 2 allDates)) ]
thursdays =  [ ("Thu",d,m,y) | (d,m,y) <- (everyf 7 (drop 3 allDates)) ]
fridays =    [ ("Fri",d,m,y) | (d,m,y) <- (everyf 7 (drop 4 allDates)) ]
saturdays =  [ ("Sat",d,m,y) | (d,m,y) <- (everyf 7 (drop 5 allDates)) ]
sundays =    [ ("Sun",d,m,y) | (d,m,y) <- (everyf 7 (drop 6 allDates)) ]

weekDates = concat [mondays, tuesdays, wednesdays, thursdays, fridays, saturdays, sundays]

sun1s = [ (wd,d,m,y) | (wd,d,m,y) <- weekDates, wd == "Sun", d == 1, y > 1900 ]

problem19 = length sun1s
-- 171

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

-- 23: Non-abundant sums
-- UNSOLVED
-- isPerfect n = (sum $ divisors n) == n
isAbundant n = (sum $ divisors n) > n
-- isDeficient n = (sum $ divisors n) < n

-- perfects = [x | x <- [1..], isPerfect x]
abundants = [x | x <- [1..], isAbundant x]

problem23 = sum [ x + x | x <- [1..28123], not (isAbundant x) ]
-- UNSOLVED

--------------------------------------------------------------------------

-- 24: Lexicographic permutations
problem24 = (sort (permutations [0,1,2,3,4,5,6,7,8,9])) !! 999999
-- 2783915460

--------------------------------------------------------------------------

-- 25: 1000-digit Fibonacci number
fibs = 1:1: zipWith (+) fibs (tail fibs)
intToList n = map ( \x -> read [x] :: Int ) ( show n )
lens = map length (map (intToList) fibs)
problem25 = (unjuster (findIndex (==1000) lens)) + 1
-- 4782

--------------------------------------------------------------------------

-- 26: Reciprocal cycles
-- UNSOLVED
intToList' n = map ( \x -> read [x] :: Int ) $ drop 2 ( show n )
-- UNSOLVED

--------------------------------------------------------------------------

-- 27: Quadratic primes
-- UNSOLVED
form1 n = (n^2) + n + 41
form2 n = (n^2) - (79*n) + 1601

formula a b n = (n^2) + (a*n) + b

coeffprod a b = a * b

check = all (==True) $ map isPrime $ map form2 [0..79]
-- check a b = count (True) $ map isPrime $ map (formula a b) [0..100]

test = [ (a,b,test1 a b) | a <- [-999..999], b <- [-1000..1000], (test1 a b)/=[] ]

test1 a b = [ formula a b n | n <- [0..100], isPrime (formula a b n) ]

form n = (n^2) - 999*n - 1000
-- UNSOLVED

--------------------------------------------------------------------------

-- 28: Number spiral diagonals

{-
21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13
-}
{-
matrix 4 4 $ \(i,j) -> 2*i - j
(  1  0 -1 -2 )
(  3  2  1  0 )
(  5  4  3  2 )
(  7  6  5  4 )
-}

m01 = matrix 5 5 $ \(row,col) -> 0
m02 = setElem 1  (3,3) m01
m03 = setElem 2  (3,4) m02
m04 = setElem 3  (4,4) m03
m05 = setElem 4  (4,3) m04
m06 = setElem 5  (4,2) m05
m07 = setElem 6  (3,2) m06
m08 = setElem 7  (2,2) m07
m09 = setElem 8  (2,3) m08
m10 = setElem 9  (2,4) m09
m11 = setElem 10 (2,5) m10
m12 = setElem 11 (3,5) m11
m13 = setElem 12 (4,5) m12
m14 = setElem 13 (5,5) m13
m15 = setElem 14 (5,4) m14
m16 = setElem 15 (5,3) m15
m17 = setElem 16 (5,2) m16
m18 = setElem 17 (5,1) m17
m19 = setElem 18 (4,1) m18
m20 = setElem 19 (3,1) m19
m21 = setElem 20 (2,1) m20
m22 = setElem 21 (1,1) m21
m23 = setElem 22 (1,2) m22
m24 = setElem 23 (1,3) m23
m25 = setElem 24 (1,4) m24
m26 = setElem 25 (1,5) m25


m5 = zero 5 5
m1001 = zero 1001 1001
m00 = matrix 5 5 $ \(row,col) -> (row,col)

-- test m 25 c r = setElem 25 (c,r) m
-- test m v  c r = test (setElem v (c,r) m) (v+1)

minr = 1
minc = 1
maxr = 5
maxc = 5
half5 = 3
half1001 = (1001 `div` 2) + 1

half = half5

goLeft  (r,c) = (r,(c-1))
goDown  (r,c) = ((r+1),c)
goRight (r,c) = (r,(c+1))
goUp    (r,c) = ((r-1),c)

nextMove (r,c) (mnr,mnc) (mxr,mxc)
  | c > mnc && r < c = goLeft (r,c)
  | r < mxr && r > c = goDown (r,c)
  | c < mxc && r > c = goRight (r,c)
  | r > mnr && r < c = goUp (r,c)
  | r == c && r < half = goDown (r,c)
  | r == c && r > half = goUp (r,c)

-- test3 (3,3) v m = setElem v (3,3) m
-- test3 (r,c) v m = test3 (nextMove (r,c)) (v-1) (setElem v (r,c) m)


-- (ROW, COLUMN)
-- ( (1,1) (1,2) (1,3) (1,4) (1,5) )
-- ( (2,1) (2,2) (2,3) (2,4) (2,5) )
-- ( (3,1) (3,2) (3,3) (3,4) (3,5) )
-- ( (4,1) (4,2) (4,3) (4,4) (4,5) )
-- ( (5,1) (5,2) (5,3) (5,4) (5,5) )

-- UNSOLVED

--------------------------------------------------------------------------

-- 29: Distinct powers
problem29 = length $ nub [ a ^ b | a <- [2..100], b <- [2..100] ]
-- 9183

--------------------------------------------------------------------------

-- 30: Digit fifth powers
pow5 ns = sum [ n ^ 5 | n <- (intToList ns) ]
isSumPow ns 
  | ns == 1 = False
  | otherwise = ns == (pow5 ns)

problem30 = sum [ x | x <- [1..355000], isSumPow x ]
-- 443839

--------------------------------------------------------------------------

-- 31: Coin sums
-- coins = [1,2,5,10,20,50,100,200]
--          a,b,c,d, e, f, g,  h
problem31 = length [ (a,b,c,d,e,f,g,h) | 
              h <- [0..1], 
              g <- [0..(2-h)], 
              f <- [0..(4-h-g)], 
              e <- [0..(10-h-g-f)],
              d <- [0..(20-h-g-f-e)], 
              c <- [0..(40-h-g-f-e-d)], 
              b <- [0..(100-h-g-f-e-d-c)], 
              a <- [0..(200-h-g-f-e-d-c-b)], 
              (a*1 + b*2 + c*5 + d*10 + e*20 + f*50 + g*100 + h*200) == 200 ] 
-- 73682

--------------------------------------------------------------------------

-- 32: Pandigital products
isPandigital n = all (==True) $ ((map (`elem` [1..l]) xs) ++ [unique xs])
  where l = length $ intToList n
        xs = intToList n

unique [n] = True
unique (n:ns) 
  | n `notElem` ns = unique ns
  | otherwise = False

uniqDigits ns = unique $ intToList ns

panDfactors x = [ (a,b) | a <- (filter (uniqDigits) (factors x)), 
                          b <- (filter (uniqDigits) (factors x)), 
                          a*b == x, 
                          not (contains0 a), not (contains0 b),
                          unique (eqToList a b x) ]

panDfacts x = take n (panDfactors x)
  where n = length (panDfactors x) `div` 2

contains0 n = 0 `elem` (intToList n)

eqToList a b c = concat $ map intToList [a,b,c]

check2 a b c = isPandigital $ listInt $ listNum $ eqToList a b c

numsWithout0 = takeWhile (<9999) [ x | x <- [1..], 0 `notElem` (intToList x) ]
numsWithPanFacts = nub [ c | c <- numsWithout0, (a,b) <- panDfacts c ]

problem32 = [ (a,b,c) | c <- numsWithPanFacts, 
                         (a,b) <- (panDfacts c), 
                         (length (eqToList a b c)) == 9]
-- 45228

--------------------------------------------------------------------------

-- 33: Digit cancelling fractions
gen2DFracs = [ (n,d) | n <- [10..99], d <- [10..99], n < d ]

fourFrac = [ (n,d) | (n,d) <- gen2DFracs, 
          (head (intToList n)) `elem` (intToList d) ||
          ((intToList n) !! 1) `elem` (intToList d), 
          ((intToList n) !! 1) /= 0 && ((intToList d) !! 1) /= 0,
          let n1 = fst(rmvNums (n,d)), 
          let d1 = snd(rmvNums (n,d)),
          (n % d) == (n1 % d1)
           ]

rmvNums (n,d) 
  |((intToList n) !! 0) == ((intToList d) !! 0) = 
                                  (((intToList n) !! 1),((intToList d) !! 1))
  |((intToList n) !! 0) == ((intToList d) !! 1) = 
                                  (((intToList n) !! 1),((intToList d) !! 0))
  |((intToList n) !! 1) == ((intToList d) !! 0) = 
                                  (((intToList n) !! 0),((intToList d) !! 1))
  |((intToList n) !! 1) == ((intToList d) !! 1) = 
                                  (((intToList n) !! 0),((intToList d) !! 0))

problem33 = product [ n % d | (n,d) <- fourFrac ]
-- 100

--------------------------------------------------------------------------

-- 34: Digit factorials
fac n = product [1..n]
facSum n = sum $ map fac (intToList n)
problem34 = [ x | x <- [3..100000], (facSum x) == x ]
-- 40730

--------------------------------------------------------------------------

-- 35: Circular primes
rotations xs = init (zipWith (++) (tails xs) (inits xs))
list2Int ns = listInt $ listNum ns
check35 n = all (isPrime) (map list2Int (rotations (intToList n)))
problem35 = length [ x | x <- (takeWhile (<1000000) primes), check35 x ]
-- 55

--------------------------------------------------------------------------

-- 36: Double-base palindromes
check36 n = isPal n && (isPal $ dec2bin n)
dec2bin x = read (showIntAtBase 2 intToDigit x "") :: Integer
problem36 = sum [ x | x <- [1..1000000], check36 x ] 
-- 872187

--------------------------------------------------------------------------

-- 37: Truncatable primes
truncLists n = tail $ filter (/=[]) ((tails $ intToList n) ++ (inits $ intToList n))
truncates n = map list2Int (truncLists n)
allPrime n = all (==True) $ map isPrime (truncates n)
problem37 = sum [ x | x <- (takeWhile (<750000) primes), allPrime x ]
-- 748334

--------------------------------------------------------------------------

-- 38: Pandigital multiples
concatProd :: Integer -> Integer -> [Integer]
concatProd x 1 = [x]
concatProd x n = concatProd x (n-1) ++ [(x*n)]

conpro :: Integer -> Integer -> Integer
conpro x n = (list2Int38 (concatProd x n))

listInteger s = read s :: Integer
list2Int38 :: [Integer] -> Integer
list2Int38 ns = listInteger $ listNum ns

is9pan :: Integer -> Bool
is9pan n = (all (==True) zs) && (length (intToList n) == 9)
  where xs = intToList n
        zs = (map (`elem` [1..9]) xs) ++ [unique xs]

problem38 = maximum [ conpro x n | x <- [1..10000], n <- [2..9], is9pan (conpro x n) ]
-- 932718654

--------------------------------------------------------------------------

-- 39: Integer right triangles

-- isPyth x y z = x^2 + y^2 == z^2

test39 p = [ (x,y,z) | x <- [1..118], y <- [1..(120-x)], z <- [1..(120-x-y)], 
                  (isPyth x y z) && (x+y+z == p),
                  x < y && y < z ]

test39x p = [ (x,y,z) | x <- [1..998], y <- [1..(1000-x)], z <- [1..(1000-x-y)], 
                  (isPyth x y z) && (x+y+z == p),
                  x < y && y < z ]

problem39 = [ (p,[x,y,z]) | p <- [3..1000], 
                  x <- [1..995], y <- [1..(1000-x)], z <- [1..(1000-x-y)], 
                  (isPyth x y z) && (x+y+z == p) ]
-- UNSOLVED

--------------------------------------------------------------------------

-- 40: Champernowne's constant
champdenom = take 1000000 $ concat $ map show [1..]
takenth n = digitToInt (champdenom !! n)
problem40 = product $ map takenth [0,9,99,999,9999,99999,999999]
-- 210

--------------------------------------------------------------------------

