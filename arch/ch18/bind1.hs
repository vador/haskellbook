
import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f as = join $ fmap f as 
