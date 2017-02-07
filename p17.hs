-- 17: Number letter counts

singles = [(1,"one"), (2,"two"), (3,"three"), (4,"four"), (5,"five"),
           (6,"six"), (7,"seven"), (8,"eight"), (9,"nine"), (0,"")]

irregulars = [(0,"ten"),(1,"eleven"),(2,"twelve"),(3,"thirteen"),
              (4,"fourteen"),(5,"fifteen"),(6,"sixteen"),(7,"seventeen"),
              (8,"eighteen"),(9,"nineteen")]

tens = [(2,"twenty"),(3,"thirty"),(4,"fourty"),(5,"fifty"),
        (6,"sixty"),(7,"seventy"),(8,"eighty"),(9,"ninety")]

hundreds n = unjust (lookup n singles) ++ "hundred"
thousands n = unjust (lookup n singles) ++ "thousand"

numList n = map ( \x -> read [x] :: Int ) ( show n )

listNum [] = []
listNum (n:ns) = (show n) ++ listNum ns

listInt s = read s :: Int

-- 123
translate [x] = unjust (lookup x singles)
translate [x,y] 
  | x == 0 = translate [y]
  | x == 1 = unjust (lookup y irregulars)
  -- | x `elem` [1..9] && y == 0 = 
  | otherwise = unjust (lookup x tens) ++ unjust (lookup y singles)
translate [x,y,z]
  | x == 0 = translate [y,z]
  | x `elem` [1..9] && y == 0 && z `elem` [1..9] = hundreds x ++ "and" ++ translate [z]
  | x `elem` [1..9] && y == 0 && z == 0 = hundreds x
  | otherwise = hundreds x ++ "and" ++ translate [y,z]
translate [w,x,y,z] = thousands w ++ translate [x,y,z]

allnumbers = concat [ translate (numList x) | x <- [1..1000]]

unjust Nothing = []
unjust (Just x) = x

main = print [ translate (numList x) | x <- [1..1000]]
