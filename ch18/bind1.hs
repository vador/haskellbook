
import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f as = join $ fmap f as 

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []
