module MaybeLib where

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just y) = f y

fromMaybe :: a -> Maybe a -> a
fromMaybe x y = mayybee x id y

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = case x of
  Nothing -> catMaybes xs
  Just s -> s : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe x = flipMaybe' x []

flipMaybe' :: [Maybe a] -> [a] -> Maybe [a]
flipMaybe' [] x = Just x
flipMaybe' (x:xs) acc = case x of
  Nothing -> Nothing
  (Just s) -> flipMaybe' xs (s : acc)
  
