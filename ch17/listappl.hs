module Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (1, (Cons a <$> arbitrary ))
              , (1, return Nil)]
    
instance Eq a => EqProp (List a) where (=-=) = eq

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  fs <*> as = fold (\f b -> append (fmap f as) b) Nil fs



append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a->b->b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap' :: (a -> List b) -> List a -> List b
flatMap' f as = (fold (\x b -> append (f x) b) Nil as)

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

-------------------------------------------------
take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons a b) = Cons a (take' (n-1) b)


newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary = fmap ZipList'  (arbitrary)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ repeat' x
  ZipList' fs <*> ZipList' as = ZipList' $
    fold (\(f,v) b -> Cons (f v) b) Nil (zip' fs as)

zip' :: List a -> List b -> List (a,b)
zip' Nil _ = Nil
zip' _ Nil = Nil
zip' (Cons a as) (Cons b bs) = Cons (a,b) (zip' as bs)

repeat' :: a -> List a
repeat' x = Cons x (repeat' x) 

toMyList = foldr Cons Nil
xs = toMyList [1,2,3]
c = Cons

ff = flatMap (\x -> x `c` (9 `c` Nil)) xs
functions = Cons (+1) (Cons (*2) Nil)
values = Cons 1 (Cons 2 Nil)


-- quickBatch $applicative [("b", "w", 1)]
z = ZipList' $ toMyList [(+9), (*2), (+8)]
