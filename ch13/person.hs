module PersoneBuild where

import System.Exit (exitSuccess)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age >0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                       "Name was: " ++ show name ++
                       "Age was: " ++ show age

gimmePerson :: IO()
gimmePerson = do
  putStr "Enter name :"
  name <- getLine
  putStr "Enter age :"
  strAge <- getLine
  let person = mkPerson name $ read(strAge) in
    case person of
      Right p -> do
        putStrLn "Yay!"
        print p
      Left p -> do
        putStrLn "Error!"
        print p

  
