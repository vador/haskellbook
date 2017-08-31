
data Constant a b =
  Constant b

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

data Two a b =
  Two a b

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = f c

filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
        => (a-> Bool) -> t a -> f a
filterF fil as = foldr (\a b -> case fil a of
                    True -> mappend (pure a) b
                    False -> b) mempty as

x :: Int-> Bool
x b = case even b of
  True -> False
  False -> True
  
