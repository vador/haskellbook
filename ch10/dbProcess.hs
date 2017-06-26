import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase:: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello World!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate (DbDate myDate:xs) = myDate : filterDbDate xs
filterDbDate (_:xs) = filterDbDate xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber (DbNumber myNum:xs) = myNum : filterDbNumber xs
filterDbNumber (_:xs) = filterDbNumber xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb  = (foldl (+) 0) . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb [] = 0
avgDb x = (fromIntegral(sumDb(x)))/ (fromIntegral(length(filterDbNumber(x))))
