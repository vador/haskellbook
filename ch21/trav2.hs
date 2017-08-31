module Travers2 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

  
instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary  = do
    a <- arbitrary
    return $ Identity a

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity $ mempty
  mappend (Identity x) (Identity y) = Identity $ mappend x y 

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative (Identity) where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity $ f x

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity b) = Identity <$> f b


instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

main = do
  let trigger :: Identity (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)
