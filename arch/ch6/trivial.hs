data Trivial =
  Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True
  
data DayOfWeek =
  Mon| Tue | Weds | Thu | Fri | Sat | Sun

data Date =
  Date DayOfWeek Int


instance Eq DayOfWeek where
  (==) Mon Mon   = True
  (==) Tue Tue   = True
  (==) Weds Weds = True
  (==) Thu Thu   = True
  (==) Fri Fri   = True
  (==) Sat Sat   = True
  (==) Sun Sun   = True
  (==) _   _     = True
  
instance Eq Date where
  (==) (Date weekDay  dayOfMonth)
       (Date weekDay' dayOfMonth') =
    weekDay == weekDay' && dayOfMonth == dayOfMonth'

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x)
       (TisAn y) =
    x == y
    
data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a b) (Two c d)=
    a == c && b == d
    

data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt b) = a == b
  (==) (TisAString x) (TisAString y) = x == y
  (==) _ _ = False 

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair c d) =
    a == c && b == d


data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple c d) =
    a == c && b == d
    
data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne b) = a == b
  (==) (ThatOne a) (ThatOne b) = a == b
  (==) _ _ = False

data EitherOr a b =
    Hello a
  | Goodbye b
  
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello y) = x == y
  (==) (Goodbye x) (Goodbye y) = x == y
  (==) _ _ = False
