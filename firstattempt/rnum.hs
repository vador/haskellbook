import Data.List

toRom :: Int -> String
toRom = toRomFromDigs . digs

-- Converts integer to list of digits
digs :: Integral x => x-> [x]
digs 0 = []
digs x = digs(x `div` 10) ++ [x `mod` 10]

-- Converts list of digits into numeral representation, decimal place by decimal place
toRomFromDigs :: [Int] -> [Char]
toRomFromDigs [] = []
toRomFromDigs (x:xs) = (digitToRom2 (length xs) x) ++ (toRomFromDigs xs)


-- Converts a tuple decimal position, digit into its roman representation
digitToRom2 :: Int -> Int -> [Char]
digitToRom2 decpos  x
	| x < 4 = replicate x valSymb1
	| x == 4 = [valSymb1,valSymb2]
	| x < 9 = valSymb2 : replicate (x-5) valSymb1
	| x == 9 = [valSymb1,valSymb3]
	where pos = decpos * 2 -- position in the symbols list for the unit symbol
	      symbols = ['I','V','X','L','C','D','M'] -- List of roman symbols by order
	      (valSymb1,valSymb2,valSymb3) = (symbols !! (pos),symbols !! (pos+1),symbols !! (pos+2))
		  -- val1 is unit, val2 is "five units" val3 is "ten units"


isLetterPrint :: Int -> (Int,Int) -> Bool
isLetterPrint idx (x, y) =  (idx-(abs(x-x0) + abs (y-x0))) == 0
	where x0 = idx
	
	
 -- map (isLetterPrint 2) [(x,y) | x <- [0..4], y<-[0..4]]
 
-- diamond :: Int -> [Bool]
-- diamond x = map isLetterPrint x zip([0..x*2] [0..x*2])

intToLetter :: Int -> Char
intToLetter x = ['A'..'Z'] !! x

letterToInt :: Char -> Maybe Int
letterToInt x = elemIndex x ['A'..'Z']

getLetter :: Int -> Int -> Bool -> Char
getLetter _ _ False = ' '
getLetter x y True = ['A'..'Z'] !! (x - abs(x - y))

diamondLine :: Int -> Int -> String
diamondLine i l = [getLetter i l $ isLetterPrint i (l,x)| x <- [0..i*2]  ]

diamond :: Char -> [String]
diamond a = map (diamondLine x ) [0..x*2]
	where x = maybe 0 $ letterToInt a