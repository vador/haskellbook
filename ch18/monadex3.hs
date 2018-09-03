module MonExo where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)


instance Functor Identity where
  fmap f (Identity x) = Identity $ f x


instance Applicative Identity where
  pure x = Identity x
  (<*>) (Identity f) (Identity x) = fmap f (Identity x)

instance Monad Identity where
  return = pure
  (>>=) (Identity x) f = f x 

instance (Arbitrary a) =>Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq 

main :: IO()
main = do
  let trigger :: Identity (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
