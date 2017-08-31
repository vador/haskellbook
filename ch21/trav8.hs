module Travers8 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bigger a b=
  Bigger a b b b
  deriving (Eq, Ord, Show)

  
instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Bigger a b) where
  -- arbitrary  = do
  --   a <- arbitrary
  --   b <- arbitrary
  --   c <- arbitrary
  --   d <- arbitrary 
  --   return $ Bigger a b c d
  arbitrary =
    Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance (Monoid a, Monoid b) => Monoid (Bigger a b) where
  mempty = Bigger mempty mempty mempty mempty
  mappend (Bigger a1 b1 c1 d1) (Bigger a2 b2 c2 d2) =
    Bigger (mappend a1 a2) (mappend b1 b2) (mappend c1 c2) (mappend d1 d2)

instance Functor (Bigger a) where
  fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance (Monoid a) => Applicative (Bigger a) where
  pure a = Bigger mempty a a a
  (<*>) (Bigger a1 f1 f2 f3) (Bigger a2 b1 b2 b3) =
    Bigger (mappend a1 a2) (f1 b1) (f2 b2) (f3 b3)

instance Foldable (Bigger a) where
  foldMap f (Bigger a b1 b2 b3) = mappend (mappend (f b1) (f b2)) (f b3)

instance Traversable (Bigger a) where
  traverse f (Bigger a b1 b2 b3) = (Bigger a) <$> (f b1) <*> (f b2) <*> (f b3)

main = do
  let trigger :: Bigger (String, String, [String])  (String, String, [String])
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
  quickBatch (traversable trigger)
  

