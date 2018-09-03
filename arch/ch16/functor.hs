{-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck
import Test.QuickCheck.Function

data Two a b =
  Two a b
  deriving (Eq, Show)

data Or a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
  
instance Functor (Or a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)


functorIdentity :: (Functor f, Eq (f a)) =>
                        f a
                     -> Bool
functorIdentity f =
  fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                       (a -> b)
                    -> (b -> c)
                    -> f a
                    -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

li x = functorCompose (+1) (*2) (x :: [Int])

functorCompose' :: (Eq (f c), Functor f) =>
                     f a
                  -> Fun a b
                  -> Fun b c
                  -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool
