--

module CorrectSyntax where

x = (+)

f xs = w `x` 1
  where w = length xs

f' (a,b) = a


