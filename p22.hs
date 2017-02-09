--22: Names scores
import Data.List

-- "MARY","PATRICIA","LINDA","BARBARA","ELIZABETH","JENNIFER","MARIA","SUSAN","MARGARET","DOROTHY","LISA"

main = do 
          contents <- readFile "p022_names.txt"
          let full = sortedList contents
          print $ sum [ score name full | name <- full ]
          
unjust Nothing = 0
unjust (Just n) = n

sortedList xs = sort (read ("[" ++ xs ++ "]") :: [String])

alphval [] = 0
alphval (x:xs) = (head [ n | (n,l) <- val, l == x]) + alphval xs

val = [(1,'A'),(2,'B'),(3,'C'),(4,'D'),(5,'E'),(6,'F'),(7,'G'),
       (8,'H'),(9,'I'),(10,'J'),(11,'K'),(12,'L'),(13,'M'),(14,'N'),
       (15,'O'),(16,'P'),(17,'Q'),(18,'R'),(19,'S'),(20,'T'),(21,'U'),
       (22,'V'),(23,'W'),(24,'X'),(25,'Y'),(26,'Z')]

pos name list = 1 + (unjust (findIndex (==name) list))

score name list 
  | (pos name list) == 0 = alphval name
  | otherwise = (pos name list) * (alphval name)

-- 871198282