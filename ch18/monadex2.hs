module MonExo where

import Prelude hiding (Either, Left, Right)
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data PhhhbbtttEither a b =
    Left a
  | Right b
  deriving (Eq, Show)


instance Functor (PhhhbbtttEither a) where
  fmap _ (Left a) = Left a
  fmap f (Right b) = Right $ f b

instance Applicative (PhhhbbtttEither a) where
  pure a = Right a
  
  (<*>) _ (Left x) = Left x
  (<*>) (Left x) _ = Left x
  (<*>) (Right f) (Right x) = Right $ f x

instance Monad (PhhhbbtttEither a) where
  return = pure
  (>>=) (Left x) _ = Left x
  (>>=) (Right x) f = f x

instance (Arbitrary a, Arbitrary b) =>Arbitrary (PhhhbbtttEither a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return $ Left a)
              , (3, return $ Right b)]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither a b) where
  (=-=) = eq 

main :: IO()
main = do
  let trigger :: PhhhbbtttEither String (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
