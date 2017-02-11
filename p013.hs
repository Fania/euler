-- 13: Large sum

main = do 
          contents <- readFile "p013.txt"
          let full = fileToList contents
          print $ take10Sum full
          
fileToList xs = read ("[" ++ xs ++ "]") :: [Integer]
take10Sum xs = read (take 10 $ show $ sum xs) :: Int

-- 5537376230