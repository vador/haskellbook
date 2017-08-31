import Control.Monad

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f as = f <$> as

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f as bs = (f <$> as) <*> bs

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m
  => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = l2 (\x y -> x:y) (f x) (meh xs f) 


flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id
