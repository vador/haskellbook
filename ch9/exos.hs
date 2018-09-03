import Data.Char

filterUpper :: String -> String
filterUpper = filter isUpper

upperFirst :: String -> String
upperFirst (x:xs) = toUpper x : xs

upperize :: String -> String
upperize [] = []
upperize (x:xs) = toUpper x : upperize xs

returnFirstUpper :: String -> Char
returnFirstUpper = (toUpper . head)
