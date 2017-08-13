--

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y
-- fill in the types
flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

sumNums :: (Eq a, Num a) => a -> a
sumNums 0 = 0
sumNums x = x + sumNums (x-1)

recMul :: (Integral a) => a -> a -> a
recMul x y = go x y 0 
   where go x y partialSum
          | x == 0 = partialSum
          | otherwise = go (x-1) y (partialSum +y)

data DividedResult =
    Result Integer
  | DividedByZero

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

mc91 :: Integral a => a -> a
mc91 x 
  | x > 100 = x - 10
  | otherwise = mc91 . mc91 $ (x+11)

  
