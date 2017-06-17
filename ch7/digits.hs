tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10
        
tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where (xLast,remLast) = x `divMod` 10
        (dLast,d)       = xLast `divMod` 10
        
foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    True -> x
    False -> y
    
foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
  | b = x
  | not b = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x,y) = (f x, y)
