import Data.Monoid
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant x) = Constant x

instance (Monoid a) => Applicative (Constant a) where
  pure e = Constant mempty
  (Constant f) <*> (Constant x) = Constant (mappend f x)

