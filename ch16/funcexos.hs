import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Eq (f a), Functor f) =>
                   f a -> Bool
functorIdentity x =
  fmap id x == x

functorCompose' :: (Eq (f c), Functor f) =>
                     f a
                  -> Fun a b
                  -> Fun b c
                  -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type LIntFC = [Int] -> IntToInt -> IntToInt -> Bool
type PairIntFC = Pair Int -> IntToInt -> IntToInt -> Bool
type ThreeIntFC = Three Char Char Int -> IntToInt -> IntToInt -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $  Identity a

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)
  
instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b
    
data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c)
  where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return $ Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
  

main :: IO ()
main = do
  quickCheck (\x -> functorIdentity (x :: Identity Int))
  quickCheck (functorCompose' :: LIntFC)
  quickCheck (\x -> functorIdentity (x :: Pair Int))
  quickCheck (functorCompose' :: PairIntFC)
  quickCheck (\x -> functorIdentity (x :: Three Char Char Int))
  quickCheck (functorCompose' :: ThreeIntFC)
