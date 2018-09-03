myAbs :: Integer -> Integer
myAbs x
  | x < 0     = (-x)
  | otherwise = x

pal xs
  | xs == reverse xs = True
  | otherwise        = False
