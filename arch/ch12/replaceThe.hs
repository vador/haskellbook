module ReplaceThe where

import Data.List

notThe :: String -> Maybe String
notThe s = case (s == "the") of
             True -> Nothing
             False -> Just s

nothingIsA :: Maybe String -> String
nothingIsA Nothing = "a"
nothingIsA (Just s) = s

replaceThe :: String -> String
replaceThe s = intercalate " " $ foldr (\w ls -> ((nothingIsA . notThe) w) : ls) [] (words s)
