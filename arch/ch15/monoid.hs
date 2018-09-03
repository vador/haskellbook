data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a=> Monoid (Optional a) where
  mempty = Only mempty
  mappend Nada (Only x) = Only x
  mappend (Only x) Nada = Only x
  mappend (Only x) (Only y) = Only $ mappend x y



