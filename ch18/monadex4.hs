module MonExo where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) $ fmap f as

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
--  (<*>) fs as = fold (\f b -> ljoin (f <$> as) b) Nil fs
  (<*>) (Cons f fs) as = (f <$> as) `ljoin` (fs <*> as)
instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons a as) f = f a `ljoin` (as >>= f)

ljoin :: List a -> List a -> List a
ljoin Nil ys = ys
ljoin (Cons x xs) ys = Cons x $ xs `ljoin` ys

foldl' :: (a -> b -> b) -> b -> List a -> b
foldl' _ b Nil = b
foldl' f b (Cons x xs) = foldl' f (f x b) xs

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons x xs) = f x (fold f b xs)

instance (Arbitrary a) =>Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (3, (Cons a <$> arbitrary))
              , (1, return Nil)]


instance (Eq a) => EqProp (List a) where
  (=-=) = eq 

main :: IO()
main = do
  let trigger :: List (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
