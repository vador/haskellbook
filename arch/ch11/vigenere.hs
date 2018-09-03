module Vigenere where

import Data.Char

shiftChar :: Int -> Char -> Char
shiftChar s c
  | 'a' <= c && c <= 'z' = chr (mod (ord c - ord 'a' + s) 26 + ord 'a')
  | 'A' <= c && c <= 'Z' = chr (mod (ord c - ord 'A' + s) 26 + ord 'A')
  | otherwise       = c

shiftChar' :: Int -> Char -> Char
shiftChar' s c
  | 'a' <= c && c <= 'z' = shiftBase 'a' s c
  | 'A' <= c && c <= 'Z' = shiftBase 'A' s c
  | otherwise       = c
    where  shiftBase base s c = chr (mod (ord c - ord base + s) 26 + ord base)
caesar :: Int -> String -> String
caesar s str = map (shiftChar' s) str

unCaesar :: Int -> String -> String
unCaesar s str = caesar (negate s) str

howManyShift :: Char -> Int
howManyShift c = ord c - ord 'A'

extractRotate :: String -> (Char,String)
extractRotate (x:xs) = (x, xs ++ [x])

vigenere :: String -> String -> String
vigenere _ [] = []
vigenere k (' ':xs) = " " ++ vigenere k xs
vigenere k (x:xs) = [shiftChar n x] ++ vigenere newkey xs
  where (charShift,newkey) = extractRotate k
        n = howManyShift charShift
