module Travers3 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Constant a b =
  Constant { getConstant :: a}
  deriving (Eq, Ord, Show)

  
instance (Arbitrary a) => Arbitrary (Constant a e) where
  arbitrary  = do
    a <- arbitrary
    return $ Constant a

instance (Monoid a) => Monoid (Constant e a) where
   mempty =  mempty
   mappend = mappend 

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance (Monoid a) => Applicative (Constant a) where
  pure e = Constant mempty
  (<*>) (Constant f) (Constant x) = Constant $ mappend f x

instance Foldable (Constant a) where
  foldMap f (Constant x) = mempty

instance Traversable (Constant a) where
  traverse f (Constant x) = pure $ Constant x


instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

main = do
  let trigger :: Constant (String, String, [String]) (Int, Int, [Int])
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (traversable trigger)
  
