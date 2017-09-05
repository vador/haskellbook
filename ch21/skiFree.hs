{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a)
         => Arbitrary (S n a) where
  arbitrary =
    S <$> arbitrary <*> arbitrary

instance ( Applicative n
         , Testable (n Property)
         , EqProp a )
         => EqProp (S n a) where
  (S x y) =-= (S p q) =
      (property $ (=-=) <$> x <*> p)
    .&. (y =-= q)

instance Functor n => Functor (S n) where
  fmap f (S na b) = S (f <$> na) (f b)

instance (Foldable n, Functor n) => Foldable (S n) where
  foldMap f (S na b) = mappend (foldMap f na) (f b)

instance Traversable n
    => Traversable (S n) where
  traverse f (S na b) = S <$> (traverse f  na) <*> (f b)



main = do
  --sample' (arbitrary :: Gen (S [] (Char,Char,String)))
  let trigger :: (S [] (Char, Char, String))
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (traversable trigger)
  
