import Data.Monoid 
import Data.Semigroup hiding ((<>))

sum :: (Foldable t, Num a) => t a -> a
--sum = foldr (+) 0
sum fs = getSum $ foldMap Sum fs 

product :: (Foldable t, Num a) => t a -> a
product fs = getProduct $ foldMap Product fs

elem :: (Foldable t, Eq a)
     => a -> t a -> Bool
--elem e fs = foldr (\x y -> (x == e) || y) False fs
elem e fs = getAny $ foldMap (\x -> Any (x == e)) fs



null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

length :: (Foldable t) => t a -> Int
length = foldr (\_ b -> b+1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (\a l -> a : l) [] 

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap' :: (Foldable t, Monoid m)
         => (a -> m) -> t a -> m
foldMap' f xs = foldr (\x b -> (f x) <> b) mempty xs

minimum :: (Ord a, Foldable t) => t a -> Maybe a
minimum as = foldr mini Nothing as

mini :: (Ord a) => a -> Maybe a -> Maybe a
mini a Nothing = Just a
mini a (Just b) = if (a < b) then Just a
                  else Just b
                      
