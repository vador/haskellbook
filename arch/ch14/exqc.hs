module QcTests where

import Test.QuickCheck
import Data.List (sort)

half :: Double -> Double
half x = x / 2

--halfIdentity :: Double -> Double
halfIdentity = (*2) . half

prop_halfdoubleId :: Double -> Bool
prop_halfdoubleId x =
  (halfIdentity x) == x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x>=y)

genOrderedLists :: (Arbitrary a, Ord a) => Gen [a]
genOrderedLists = do
  al <- arbitrary
  return $ sort al

genList :: (Arbitrary a) => Gen [a]
genList = do
  al <- arbitrary
  return al

listOrderedInt :: [Int] -> Bool
listOrderedInt = listOrdered

prop_sortedIntList :: Property
prop_sortedIntList =
  forAll (genOrderedLists :: Gen [Int])
  (\c -> listOrdered c)

plusAssociative x y z =
  x + (y + z) == (x + y) +z

plusCommutative x y =
  x + y == y + x


quotRemId x (Positive y) =
  (quot x y)*y + (rem x y) == x

square x = x * x

data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

instance Arbitrary Fool where
  arbitrary = genFool

genFool' :: Gen Fool
genFool' = frequency [(1, return $ Fulse), (2, return $ Frue)]
