module Tree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = do
    
    frequency  $ [ (1, return Empty)
                 , (1, Leaf <$> arbitrary)
                 , (1, Node <$> arbitrary <*> arbitrary <*> arbitrary)] 
    
instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq


instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node l a r) = Node (f <$> l) (f a) (f <$> r)

instance Applicative Tree  where
  pure x = Node (pure x) x (pure x)
  (<*>) Empty _ = Empty
  (<*>) _ Empty = Empty
  (<*>) (Leaf f) (Leaf x) = Leaf (f x)
  (<*>) (Node l f r) (Leaf x) = Leaf (f x)
  (<*>) (Leaf f) (Node l x r) = Node Empty (f x) Empty
  (<*>) (Node l f r) (Node xl x xr) = Node (l <*> xl) (f x) (r <*> xr)

  
instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node l a r) = mappend (foldMap f l) (mappend (f a) (foldMap f r))

  foldr _ b Empty = b
  foldr f b (Leaf a) = f a b
  foldr f b (Node l a r) = foldr f (foldr f (f a b) l) r

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> (f x)
  traverse f (Node r x l) = Node <$> (traverse f r) <*> (f x) <*> (traverse f r)
  
main = do
  let trigger :: (Tree (Char, Char, String))
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (traversable trigger)
  

