stops = "pbtdkg"
vowels = "aeiou"

stopvowstop = [[x,y,z] | x <-stops,  y <-vowels, z<-stops, x=='p']

seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))
