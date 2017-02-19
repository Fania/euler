

isPalindrome :: Show a => a -> Bool
isPalindrome n = ns == reverse ns
  where ns = show n

reverseNum :: Show a => a -> Int
reverseNum n = read (reverse $ show n) :: Int 

check n = isPalindrome $ n + (reverseNum n)