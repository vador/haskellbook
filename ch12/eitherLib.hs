module EitherLib where

lefts' :: [Either a b] -> [a]
lefts' = foldr (\x acc -> case x of
                   (Left a) -> a : acc
                   (Right b) -> acc) []
  
right' :: [Either a b] -> [b]
right' = foldr (\x acc -> case x of
                   (Left a) ->  acc
                   (Right b) -> b : acc) []
  
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr (\x (acc1,acc2) -> case x of
                                               (Left a) ->  (a : acc1, acc2)
                                               (Right b) -> (acc1, b : acc2)) ([],[])

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\x -> Nothing) (\x -> Just (f x))
