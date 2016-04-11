import Data.Char


isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf (x:xs) [] = False
isSubsequenceOf [] _ = True
isSubsequenceOf xsa@(x1:xs1) (x2:xs2) = if x1 == x2 
                        then isSubsequenceOf xs1 xs2
                        else isSubsequenceOf xsa xs2
                        
capitalizeWord :: String -> String
capitalizeWord w 
   | null w = w
   | firstLetter == ' ' = [' '] ++ capitalizeWord others
   | otherwise = [toUpper firstLetter] ++ map toLower others
   where ([firstLetter],others) = splitAt 1 w

myWords :: String -> [String]
myWords [] = []
myWords (' ':xs) = myWords xs
myWords xs =
  takeWhile (/=' ') xs: myWords 
    (dropWhile (/= ' ') xs)
 
mySentences :: String -> [String]
mySentences [] = []
mySentences ('.':xs) = mySentences xs
mySentences xs =
  (takeWhile (/='.') xs ++ "."): mySentences (dropWhile (/= '.') xs)  

capitalizeWords :: String -> [(String,String)]
capitalizeWords xs = map (\x -> (x, capitalizeWord x)) $ myWords xs

capitalizeSentences :: String -> String
capitalizeSentences xs = foldr (\x acc -> capitalizeWord x ++ acc) "" $ 
  mySentences xs 