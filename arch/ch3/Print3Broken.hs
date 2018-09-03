-- print3Broken.hs

module Print3Brocken where

printSecond :: String -> IO ()
printSecond xs = do
  putStrLn xs

main :: IO ()
main = do
  putStrLn greeting
  printSecond greeting
  where greeting = "Yarrrr"

 
