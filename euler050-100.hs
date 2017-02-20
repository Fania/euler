import Data.List
import Data.Numbers.Primes

--------------------------------------------------------------------------

-- 50: Consecutive prime sum

primeSum n =  [ (foldl1 (+) (takeWhile (<100) primes)) ]

succPrime n 
  | isPrime n = Just (head $ dropWhile (<=n) primes)
  | otherwise = Nothing

isSuccPrimeSeq [] = False
isSuccPrimeSeq [x] = True
isSuccPrimeSeq (x:xs) 
  | unjust50 (succPrime x) == head xs = isSuccPrimeSeq xs
  | otherwise = False

unjust50 Nothing = 0
unjust50 (Just x) = x

-- [2,3,5,7,11,13,17,19,23,29]
-- [2]
-- [2,3]
-- [5,7,11]


primeSubSeq = filter isSuccPrimeSeq $ subsequences (takeWhile (<1000) primes)

-- primeSubSeqSums = [ (l,p) | x <- primeSubSeq, 
--                                       let p = sum x,
--                                       let l = length x ]
-- UNSOLVED

--------------------------------------------------------------------------

-- 51: Prime digit replacements

twoDigitPrimes = dropWhile (<10) $ takeWhile (<100) primes

listNum [] = []
listNum (n:ns) = (show n) ++ listNum ns
listInt s = read s :: Int
intToList n = map ( \x -> read [x] :: Int ) ( show n )
listToInt ns = listInt $ listNum ns

test51 = [ (x,y) | [x,y] <- (map intToList twoDigitPrimes) ]

test51a = [ x | (x,y) <- test51, let z = listToInt [x,y]   ]
-- UNSOLVED

--------------------------------------------------------------------------

-- 52: Permuted multiples

isPermutation n m = xs `elem` (permutations ys)
  where xs = intToList n
        ys = intToList m


problem52 = head [ x | x <- [1..], 
                  isPermutation x (x*2),
                  isPermutation x (x*3),
                  isPermutation x (x*4),
                  isPermutation x (x*5),
                  isPermutation x (x*6)]
-- 142857

has_same_digits a b = (show a) \\ (show b) == []
check n = all (has_same_digits n) (map (n*) [2..6])
problem_52 = head $ filter check [1..]

--------------------------------------------------------------------------

-- 53: Combinatoric selections
fact n = product [1..n]

isOrdered xs = and $ zipWith (<) xs (tail xs)

comb x y = map (listToInt) $ filter (isOrdered) $ nub $ map (take y) $ permutations $ intToList x

ncr n r = (fact n / fact r) / fact (n-r)

problem53 = length $ filter (>1000000) [ ncr n r | n <- [1..100], r <- [1..n] ]
-- 4075

--------------------------------------------------------------------------

-- 54: Poker hands

data CardValue = Two | Three | Four | Five | Six | Seven | Eight |
            Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord, Read, Enum, Bounded)

data Suit = Spade | Club | Heart | Diamond
  deriving (Show, Read)

data Card = Card Suit CardValue
  deriving (Show, Read)

newtype Hand = Hand [Card]
  deriving (Show, Read)

x = Hand [Card Spade Two, Card Club Four, Card Heart Ace, Card Heart King, Card Diamond Jack]

-- onePair (Hand [c]) = False
-- onePair (Hand (c:cs)) = 

-- 8C TS KC 9H 4S 7D 2S 5D 3S AC
-- 5C AD 5D AC 9C 7C 5H 8D TD KS