i :: a -> a
i = id

c :: a -> b -> a
c a b = a

cp :: a -> b -> (a,b)
cp = (,)



c' :: a -> b -> b
c' a b = b

co :: (b -> c) -> (a -> b) -> a -> c
co f g x = f( g x) 
