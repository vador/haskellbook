myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
  if x == False
  then False
  else myAnd xs

myAnd' :: [Bool] -> Bool
myAnd' [] = True
myAnd' (x:xs) = x && myAnd xs

myAnd'' :: [Bool] -> Bool
myAnd'' = foldr (\a b ->
                  if a == False
                  then False
                  else b) True

myAnd''' :: [Bool] -> Bool
myAnd''' = foldr (&&) True

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
  if x == True
  then True
  else myOr xs

myOr' :: [Bool] -> Bool
myOr' [] = False
myOr' (x:xs) = x || myOr' xs

myOr'' :: [Bool] -> Bool
myOr'' = foldr (\a b ->
                  if a == True
                  then True
                  else b) False

myOr''' :: [Bool] -> Bool
myOr''' = foldr (||) False


myAny :: (a -> Bool) -> [a]-> Bool
myAny _ [] = False
myAny f (x:xs) =
  if f x == True
  then True
  else myAny f xs

myAny' :: (a -> Bool) -> [a]-> Bool
myAny' _ [] = False
myAny' f (x:xs) = (f x) || myAny f xs

myAny'' :: (a -> Bool) -> [a]-> Bool
myAny'' f = foldr (\a b ->
                     if f a == True
                     then True
                     else b) False

--myAny''' :: (a -> Bool) -> [a]-> Bool
--myAny''' f = foldr ((||) . f)

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b ->
                    if a == x
                    then True
                    else b) False
                   
myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (==x)

myReverse :: [a] -> [a]
myReverse = foldl (\a b ->
                     b : a) []

myMap :: (a->b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []

myFilter :: (a -> Bool) ->  [a] -> [a]
myFilter f = foldr (\a b ->
                      if (f a == True)
                      then a : b
                      else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap (id)

myMaximumBy :: (a-> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr (\a b ->
                         if f a b == GT
                         then a
                         else b) x (x:xs) 
                       
