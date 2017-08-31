module Travers4 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

  
instance (Arbitrary a) => Arbitrary (List a) where
   arbitrary  = do
    a <- arbitrary
    frequency [ (1 , return Nil)
              , (3, (Cons a <$> arbitrary ))]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance Monoid (List a) where
  mempty =  Nil
  mappend xs Nil = xs
  mappend Nil ys = ys
  mappend (Cons x xs) ys = Cons x (mappend xs ys)

instance Functor (List) where
  fmap _ Nil    = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil  _    = Nil
  (<*>) (Cons f fs) as = mappend (f <$> as) (fs <*> as) 

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = mappend (f x) (foldMap f xs)

instance Traversable List where
  traverse f xs = foldr (\x ys -> Cons <$> (f x) <*> ys) (pure Nil) xs

main = do
  let trigger :: List (String, String, [String]) 
      trigger = undefined
--  quickBatch (functor trigger)
--  quickBatch (applicative trigger)
  quickBatch (traversable trigger)
  

