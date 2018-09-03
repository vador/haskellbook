-- takeSome.hs

module TakeSome where

funcA :: String -> String
funcA x = take 16 x

funcB :: String -> String
funcB x = drop 4 $ take 5 x

funcC :: String -> String
funcC x = drop 9 x

thirdLetter :: String -> Char
thirdLetter s = head $ drop 2 s

thirdLetter' :: String -> Char
thirdLetter' s = s !! 2

main :: IO ()
main = do
  putStrLn $ funcA "Curry is awesome!"
  putStrLn $ funcB "Curry is awesome!"
  putStrLn $ funcC "Curry is awesome!"
  putChar $ thirdLetter "abcdef"
  putStrLn ""
  putChar $ thirdLetter "abcdef"
  putStrLn ""
