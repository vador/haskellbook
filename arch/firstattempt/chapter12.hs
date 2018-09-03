-- chapter12

notThe :: String -> Maybe String
notThe s 
  | s == "the" = Nothing
  | otherwise = Just s

myWords :: String -> [String]
myWords = undefined

replaceThe :: String -> String
replaceThe [] = []
replaceThe (' ':xs) = ' ' : replaceThe xs
replaceThe xs = 
  (idExceptThe (takeWhile (/= ' ') xs)) ++ 
    (replaceThe (dropWhile (/= ' ') xs))
  where idExceptThe xs
          | notThe xs == Nothing = "a"
          | otherwise = xs
 
isVowel :: Char -> Bool
isVowel c = elem c "aeiou"


countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel (' ':xs) = countTheBeforeVowel xs
countTheBeforeVowel xs = score word nextLetter + countTheBeforeVowel others
  where 
    word = takeWhile (/= ' ') xs
    others = dropWhile (/=' ') xs
    nextLetter = head $ dropWhile (== ' ') others
    score word nextLetter = 
      if (notThe word == Nothing) && isVowel nextLetter 
        then 1
        else 0

-- As natural as any competitive bodybuilder
data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | i == 0 = Just Zero
  | otherwise = Just (Succ (nati i)) 
    where
      nati i = 
        case integerToNat i of
          Just i -> i
          Nothing -> Zero

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just a) = True

isNothing :: Maybe a -> Bool
isNothing a = not (isJust a)

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a b = mayybee a (id) b

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes xs = foldr (\a b -> (maybeToList a) ++ b) [] xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = foldr flipMaybe' (Just []) xs

flipMaybe' :: Maybe a -> Maybe [a] -> Maybe [a]
flipMaybe' Nothing _ = Nothing
flipMaybe' _ Nothing = Nothing
flipMaybe' (Just a) (Just xs) = Just (a:xs)

-- either
bb = [Left 1, Right 0, Left 2]


left' :: Either a b -> [a]
left' (Left x) = [x]
left' _ = []

lefts' :: [Either a b] -> [a]
lefts' xs = foldr (\a b -> left' a ++ b) [] xs

lefts'' :: [Either a b] -> [a]
lefts'' xs = foldr (either left right ) [] xs
                  where
                    left a ~l = a : l
                    right b ~l = l
rights' xs = foldr (either left right ) [] xs
                  where
                    left a ~l =  l
                    right b ~l = b : l

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' xs = foldr (either left right ) ([],[]) xs
                            where
                              left a ~(l,r) = (a:l ,r)
                              right b ~(l,r) = (l, b:r)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left a) = Nothing
eitherMaybe' f (Right b) = Just (f b) 
                              
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x = either' (\_ -> Nothing) (Just . f)  x 

-- anamorphism

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a) 

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
                  Just (a,b) -> a : myUnfoldr f b
                  otherwise  -> []
 
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f(b))) x

-- biarytree

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = case f x of
               Just (a, b, c) -> Node (unfold f a) b (unfold f c)
               Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold 
                (\i -> if i<= n then Just (i+1,i,i+1) else Nothing) 0   