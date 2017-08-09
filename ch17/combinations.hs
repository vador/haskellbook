import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos = liftA3 aba

aba :: a -> b -> c -> (a, b, c)
aba x y z = (x, y, z)
