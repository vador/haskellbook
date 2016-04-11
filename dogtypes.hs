myPug = PugData :: PugType
myHusky :: HuskyType a
myHusky = HuskyData
myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData
myOtherOtherHusky :: HuskyType [[[[[[Int]]]]]]
myOtherOtherHusky = HuskyData
-- no witness to the contrary


data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)
  
 