-- 42: Coded triangle numbers

import Data.List

main = do 
  contents <- readFile "p042_words.txt"
  let full = read ("[" ++ contents ++ "]") :: [String]
  print $ length [ y | x <- full, let y = alphval x, isTriangle y ]

alphval [] = 0
alphval (x:xs) = (head [ n | (n,l) <- val, l == x]) + alphval xs

val = [(1,'A'),(2,'B'),(3,'C'),(4,'D'),(5,'E'),(6,'F'),(7,'G'),
       (8,'H'),(9,'I'),(10,'J'),(11,'K'),(12,'L'),(13,'M'),(14,'N'),
       (15,'O'),(16,'P'),(17,'Q'),(18,'R'),(19,'S'),(20,'T'),(21,'U'),
       (22,'V'),(23,'W'),(24,'X'),(25,'Y'),(26,'Z')]

tnum 0 = 0
tnum x = x + tnum (x-1)
tnums = [ tnum x | x <- [1..] ]

isTriangle n = n `elem` (takeWhile (<200) tnums)

-- 162