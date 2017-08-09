
data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
  fmap g (LiftItOut f) = LiftItOut (fmap g f)
