module MonExo where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)


instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad (Nope) where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq 

main :: IO()
main = do
  quickBatch (functor (NopeDotJpg :: Nope (Int,Int,Int)))
  quickBatch (applicative (NopeDotJpg :: Nope (Int,Int,Int)))
  quickBatch (monad (NopeDotJpg :: Nope (Int,Int,Int)))
