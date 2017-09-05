{-# LANGUAGE FlexibleInstances #-}
module TooMany where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany = (> 42)


instance TooMany (Int, Int) where
  tooMany (a, b) = tooMany (a+b)

instance (Num a, TooMany a) => TooMany (a,a) where
  tooMany (a,b) = tooMany a && tooMany b
  
