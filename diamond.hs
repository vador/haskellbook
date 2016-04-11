import Data.List

isLetterPrint :: Int -> (Int,Int) -> Bool
isLetterPrint idx (x, y) =  (idx-(abs(x-x0) + abs (y-x0))) == 0
	where x0 = idx
	

intToLetter :: Int -> Char
intToLetter x = ['A'..'Z'] !! x

letterToMaybeInt :: Char -> Maybe Int
letterToMaybeInt x = elemIndex x ['A'..'Z']

maybeLetterToInt :: Maybe Int -> Int
maybeLetterToInt (Just x) = x
maybeLetterToInt _ = 0

getLetter :: Int -> Int -> Bool -> Char
getLetter _ _ False = ' '
getLetter x y True = ['A'..'Z'] !! (x - abs(x - y))

diamondLine :: Int -> Int -> String
diamondLine i l = [getLetter i l $ isLetterPrint i (l,x)| x <- [0..i*2]  ]

diamond :: Char -> [String]
diamond a = map (diamondLine x ) [0..x*2]
	where x = maybeLetterToInt (letterToMaybeInt a)