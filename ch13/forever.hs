module Forever where

import Control.Monad
import System.Exit (exitSuccess)

palindromeWin :: String -> IO ()
palindromeWin line1 =
  case (line1 == reverse line1) of
    True -> do
      putStrLn "It's a palindrome!"
      exitSuccess
    False -> do
      putStrLn "Nope!"
      return ()
      

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  palindromeWin line1

