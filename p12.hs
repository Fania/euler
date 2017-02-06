-- 12: Highly divisible triangular number

tnum 0 = 0
tnum x = x + tnum (x-1)
tnums = [ tnum x | x <- [1..] ]

factors n = [x | x <- [1..n], n `mod` x == 0]

main = print $ head [ x | x <- tnums, length (factors x) > 500 ]