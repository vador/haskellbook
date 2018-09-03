module DaPhone where

import Data.List
import Data.Char

type Digit = Char

type Key = (Digit, [Char])

data DaPhone = DaPhone [Key]

type Presses = Int

pressesFromKey :: Key -> Char -> Maybe Presses
pressesFromKey (d,s) c = elemIndex c s

isUpperChar :: Char -> Bool
isUpperChar c = (c >= 'A') && (c <= 'Z')

reverseTapsLower :: DaPhone -> Char -> [(Digit, Presses)]
reverseTapsLower (DaPhone (key@(k,s):phone)) c = case pressesFromKey key c of
  Just i -> [(k,i+1)]
  Nothing -> reverseTapsLower (DaPhone phone) c

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps dp c = case isUpper c of
  True  ->  [('*' :: Digit,1)] ++ (reverseTapsLower dp (toLower c))
  False -> reverseTapsLower dp c
  

cellPhonesDead :: DaPhone -> String ->  [(Digit, Presses)]
cellPhonesDead _ [] = []
cellPhonesDead dp (x:xs) = (reverseTaps dp x) ++ cellPhonesDead dp xs
  
daPhone = DaPhone [
  ('1',"1"),
  ('2',"abc2"),
  ('3',"def3"),
  ('4',"ghi4"),
  ('5',"jkl5"),
  ('6',"mno6"),
  ('7',"pqrs7"),
  ('8',"tuv8"),
  ('9',"wxyz9"),
  ('*',"^*"),
  ('0',"+ 0"),
  ('#',".,#")
  ] 
