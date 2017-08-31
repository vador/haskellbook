module Travers6 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a b=
  Pair a b
  deriving (Eq, Ord, Show)

  
instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Pair a b) where
  arbitrary  = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty = Pair mempty mempty
  mappend (Pair a1 b1) (Pair a2 b2) =
    Pair (mappend a1 a2) (mappend b1 b2)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance (Monoid a) => Applicative (Pair a) where
  pure = Pair mempty
  (<*>) (Pair a1 f) (Pair a2 b) =
    Pair (mappend a1 a2) (f b)

instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = (Pair a) <$> f b

main = do
  let trigger :: Pair (String, String, [String])  (String, String, [String])
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
  quickBatch (traversable trigger)
  

