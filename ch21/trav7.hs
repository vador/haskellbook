module Travers7 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Big a b=
  Big a b b
  deriving (Eq, Ord, Show)

  
instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Big a b) where
  arbitrary  = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary 
    return $ Big a b c

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance (Monoid a, Monoid b) => Monoid (Big a b) where
  mempty = Big mempty mempty mempty
  mappend (Big a1 b1 c1) (Big a2 b2 c2) =
    Big (mappend a1 a2) (mappend b1 b2) (mappend c1 c2)

instance Functor (Big a) where
  fmap f (Big a b c) = Big a (f b) (f c)

instance (Monoid a) => Applicative (Big a) where
  pure a = Big mempty a a
  (<*>) (Big a1 f1 f2) (Big a2 b1 b2) =
    Big (mappend a1 a2) (f1 b1) (f2 b2)

instance Foldable (Big a) where
  foldMap f (Big a b1 b2) = mappend (f b1) (f b2)

instance Traversable (Big a) where
  traverse f (Big a b1 b2) = (Big a) <$> (f b1) <*> (f b2)

main = do
  let trigger :: Big (String, String, [String])  (String, String, [String])
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
  quickBatch (traversable trigger)
  

