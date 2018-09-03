module AplExos where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck (Gen, Arbitrary, arbitrary, frequency, sample)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a = Pair a a deriving (Eq,Show)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b
    
instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

instance Functor (Pair) where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative (Pair) where
  pure x = Pair x x
  (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)

data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b 

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Monoid e) => Applicative (Two e) where
  pure = Two mempty
  (<*>) (Two e1 f) (Two e2 x) = Two (e1 `mappend` e2) (f x) 

