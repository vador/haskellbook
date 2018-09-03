
--import Data.Monoid
import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

newtype Identity a = Identity a
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x<>y)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool 

data Two a b = Two a b
  deriving (Eq,Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b 

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a1 b1 c1) <> (Three a2 b2 c2) = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

type BoolConjAssoc =  BoolConj -> BoolConj -> BoolConj -> Bool

newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

type BoolDisjAssoc =  BoolDisj -> BoolDisj -> BoolDisj -> Bool

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Fst _) <> x = x
  (Snd x) <> _ = Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)

f = Combine $ \n -> Sum (n+1)
g = Combine $ \n -> Sum (n-1)

testComb :: Combine a b -> Combine a b -> Combine a b
testComb (Combine f) (Combine g) = Combine f

newtype Comp a =
  Comp { unComp :: (a -> a)}
  
instance (Semigroup a) => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

f' = Comp (\n -> (n+ Sum 1))
g' = Comp (\n -> (n- Sum 1))

data Validation a b =
  Failure' a | Success' b
  deriving (Eq, Show)

instance Semigroup a =>
  Semigroup (Validation a b) where
    (Failure' x) <> (Failure' y) = Failure' $ x <> y
    (Success' x) <> _ = Success' x
    _ <> (Success' y) = Success' y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = genValidation

genValidation :: (Arbitrary a, Arbitrary b) => Gen (Validation a b) 
genValidation = do
    x <- (arbitrary)
    y <- (arbitrary)
    oneof [return $ Failure' x, return $ Success' y]

type ValidationAssoc a b =  Validation a b
                         -> Validation a b
                         -> Validation a b
                         -> Bool

newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b =>
  Semigroup (AccumulateRight a b) where
    (AccumulateRight (Success' x)) <> (AccumulateRight (Success' y)) =
      AccumulateRight $ Success' $ x<>y
    _ <> ar@(AccumulateRight (Success' y)) = ar
    al@(AccumulateRight (Success' x)) <> _ = al
    _ <> ar = ar

genAccumulateRight :: (Arbitrary a, Arbitrary b) => Gen (AccumulateRight a b)
genAccumulateRight = do
  x <- genValidation
  return $ AccumulateRight x

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = genAccumulateRight


type AccumulateRightAssoc a b =  AccumulateRight a b
                              -> AccumulateRight a b
                              -> AccumulateRight a b
                              -> Bool


newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (AccumulateBoth x) <> (AccumulateBoth y) = AccumulateBoth (x <> y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = genAccumulateBoth
    
genAccumulateBoth :: (Arbitrary a, Arbitrary b) => Gen (AccumulateBoth a b)
genAccumulateBoth = do
  x <- genValidation
  return $ AccumulateBoth x

type AccumulateBothAssoc a b =  AccumulateBoth a b
                             -> AccumulateBoth a b
                             -> AccumulateBoth a b
                             -> Bool
  
main :: IO()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc String)
  quickCheck (semigroupAssoc :: TwoAssoc String String)
  quickCheck (semigroupAssoc :: ThreeAssoc String String String)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc String String)
  quickCheck (semigroupAssoc :: ValidationAssoc String String)
  quickCheck (semigroupAssoc :: AccumulateRightAssoc String String)
  verboseCheck (semigroupAssoc :: AccumulateBothAssoc String String)

