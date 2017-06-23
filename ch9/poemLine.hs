module PoemLines where

firstSen = "Tyger Tyger, burning brigth\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = myWords '\n'

myWords :: Char -> String -> [String]
myWords c s = go c s [] where
  go c "" acc = reverse acc
  go c s acc = go c (rest c s ) ((first c s): acc)
    where rest c s = dropWhile (==c) (dropWhile (/=c) s)
          first c s = takeWhile (/=c) s


shouldEqual = ["Tyger Tyger, burning brigth",
               "In the forests of the night",
               "What immortal hand or eye",
               "Could frame thy fearful symmetry?"
              ]

main :: IO ()
main =
  print $ "Are they equal ?"
            ++ show (myLines sentences == shouldEqual)
            
