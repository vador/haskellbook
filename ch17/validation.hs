module Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck (Gen, Arbitrary, arbitrary, frequency, sample)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return $ Failure a)
              , (3, return $ Success b)]
      
instance (Eq a, Eq b) => EqProp (Validation a b) where
  (=-=) = eq

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success x) = Success (f x)

instance Monoid e =>
         Applicative (Validation e) where
  pure  = Success
  (<*>) (Success f) (Success x)  = Success (f x)
  (<*>) (Failure f1) (Failure f2) = Failure (f1 `mappend` f2)
  (<*>) (Failure f1) _ = Failure f1
  (<*>) _ (Failure f2) = Failure f2
  
-- quickBatch $ applicative (ZipList' (Cons (1,2,3) Nil))
data Errors =
    DividedByZero
  | StackOverFlow
  | MooglesChewedWires
  deriving (Eq, Show)

--success = Success (+1) <*> Success 1
--failure = Success (+1) <*> Failure [StackOverFlow]
