module StdFunc where

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: ( a-> Bool ) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = f x || myAny f xs 


myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = (x == a) || myElem a xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' a xs = myAny (==a) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ x : []

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = (f x) ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) = if (f x y) == GT then x else y where
  y = myMaximumBy f xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy f (x:xs) = if (f x y) == LT then x else y where
  y = myMinimumBy f xs
