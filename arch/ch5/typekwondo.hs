data Woot
data Blah

f1 :: Woot -> Blah
f1 = undefined

gh :: (Blah, Woot) -> (Woot, Blah)
gh x = ((,) $ snd x ) (fst x)
                   
g1 :: (Blah, Woot) -> (Blah, Blah)
g1 (b,w) = (b, f1 w)

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = (g . f) 

data A
data B
data C

q :: A-> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q
