module Travers4 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Three a b c =
  Three a b c
  deriving (Eq, Ord, Show)

  
instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary  = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend (Three a1 b1 c1) (Three a2 b2 c2) =
    Three (mappend a1 a2) (mappend b1 b2) (mappend c1 c2)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three a1 b1 f) (Three a2 b2 c) =
    Three (mappend a1 a2) (mappend  b1 b2) (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = (Three a b) <$> f c

main = do
  let trigger :: Three (String, String, [String])  (String, String, [String])  (String, String, [String]) 
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
  quickBatch (traversable trigger)
  

