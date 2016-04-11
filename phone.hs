-- phone

import Data.Char
import Data.List

data DaPhone = DaPhone [(Int, String)] deriving Show

stdPhone :: DaPhone
stdPhone = DaPhone [
    (0,"+ 0"),
    (1,"1"),
    (2,"ABC2"),
    (3,"DEF3"),
    (4,"GHI4"),
    (5,"JKL5"),
    (6,"MNO6"),
    (7,"PQRS7"),
    (8,"TUV8"),
    (9,"WXYZ9"),
    (10,"^*"),
    (11,".,#")
  ]


convo :: [String]
convo =
  ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]
-- validButtons = "1234567890*#"
type Digit = Int

-- Valid presses: 1 and up
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps p c
  | toUpper c == c = keyPhone p '^' : keyPhone p c : []
  | otherwise = keyPhone p (toUpper c) : []

-- assuming the default phone definition
-- 'a' -> ('2', 1)
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead (DaPhone []) _ = []
cellPhonesDead _ [] = []
cellPhonesDead p (x:xs) = reverseTaps p x ++ cellPhonesDead p xs

keyPhone :: DaPhone-> Char -> (Digit, Presses)
keyPhone (DaPhone p) c = (key, presses)
  where 
    foundIn = filter (\(keypad, values) -> elem c values) p
    (key, values) = head foundIn
    presses = (+1) $ head $ elemIndices c values

fingerTaps :: DaPhone -> String -> Presses
fingerTaps p s = foldr 
              (\(key,presses) acc -> acc + presses) 0 
              $ cellPhonesDead p s
              
sortedLetters xs = map (\letter@(x:xs) -> (x, length letter)) (groupBy (==) $ sort xs)      
            
mostUsedLetter xs = fst $ head $ sortedLetters xs

lettersCost :: DaPhone -> String -> [(Char, Presses)]
lettersCost p s = map 
  (\(c, n) -> (c, (*) n $ sum $ map snd $ reverseTaps p c)) 
  (sortedLetters s)