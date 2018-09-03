module Unfoldr where

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe(a,b)) -> b -> [a]
myUnfoldr f b = case (f b) of
  Nothing -> []
  Just (x,y) -> x : (myUnfoldr f y)

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b,  f b))
