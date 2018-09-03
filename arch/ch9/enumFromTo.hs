eftBool :: Bool -> Bool -> [Bool]
eftBool b1 b2 = go b1 b2 [] where
  go b1 b2 acc
    | b1 == b2 = (b1 : acc)
    | b1 <= b2 = go  (succ b1) b2 (b1 : acc)
    | otherwise = acc

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd o1 o2  = go o1 o2 [] where
  go b1 b2 acc
    | b1 == b2 = (b1 : acc)
    | b1 <= b2 = go  (succ b1) b2 (b1 : acc)
    | otherwise = acc

eftInt :: Int -> Int -> [Int]
eftInt = eftEnum

eftEnum :: (Ord a, Enum a) => a -> a -> [a]
eftEnum o1 o2  = go o1 o2 [] where
  go b1 b2 acc
    | b1 == b2 = reverse (b1 : acc)
    | b1 <= b2 = go  (succ b1) b2 (b1 : acc)
    | otherwise = acc

