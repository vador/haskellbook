-- exo1.hs

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g x = fst $ g $ f x


data X
data Y
data Z
xz :: X -> Z
xz = undefined
yz :: Y -> Z
yz = undefined
xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y 

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i x =  if i > 1 
                  then f x 
                  else (f x) * 2

f :: (a,b,c) -> (d,e,f) -> ((a,d),(c,f))
f (a,b,c) (d,e,f) =  ((a,d),(c,f))