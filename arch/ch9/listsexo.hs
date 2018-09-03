myfilter :: String -> [String]
myfilter = (filter (\x -> not (elem x ["the","a","an"])) . words)

myzip :: [a] -> [b] ->[(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

myzipwith:: (a->b->c) -> [a] -> [b] -> [c]
myzipwith _ [] _ = []
myzipwith _ _ [] = []
myzipwith f (x:xs) (y:ys) = (f x y) : (myzipwith f xs ys)

myzip2 :: [a] -> [b] ->[(a,b)]
myzip2 x y = myzipwith (\x y -> (x,y)) x y

