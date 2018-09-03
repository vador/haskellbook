
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf sq@(s:ss) t@(c:cs)
  | (s == c) = isSubsequenceOf ss cs
  | otherwise = isSubsequenceOf sq cs
