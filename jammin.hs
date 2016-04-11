--

module Jammin where

import Data.List

data Fruit = 
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Ord, Show)
  
--data JamJars =
--  Jam Fruit Int
--  deriving (Eq, Show)
  
data JamJars =
  Jam { fruit :: Fruit
          , jars :: Int}
  deriving (Eq, Show)

instance Ord JamJars where
  compare a b = compare (jars a) (jars b)


compareKind (Jam k _) (Jam k' _) = compare k k'  

groupJam xs = groupBy (\x y -> (==) EQ $ compareKind x y) 
                      (sortBy compareKind xs)


mostRow :: [JamJars] -> JamJars
mostRow xs = foldr max (head xs) (tail xs) 
 
row1 = Jam Peach 3
row2 = Jam Apple 5
row3 = Jam Blackberry 1
row4 = Jam Peach 4
row5 = Jam Apple 3
 
allJam = [row1, row2, row3, row4,row5]
 