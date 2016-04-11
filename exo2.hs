stops = "pbtdkg"

vowels = "aeiou"

combine :: [a] -> [b] -> [c] -> [(a,b,c)]
combine [] _ _ = []
combine _ [] _ = []
combine _ _ [] = []
combine (x:xs) (y:ys) (z:zs) = (x,y,z) :
                        combine [x] [y] (zs) ++
                        combine (xs) [y] (z:zs) ++
                        combine (x:xs) (ys) (z:zs)
                        
combinel :: [a] -> [b] -> [c] -> [(a,b,c)]
combinel a b c = [(x,y,z) | x<-a, y<-b, z<-c]
                        