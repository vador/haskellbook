-- Moods.hs

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood    _ = Blah

area :: Double -> Double
area r = pi * (r * r)
