stmt = "all i wanna do is have some fun"

myWords :: String -> [String]
myWords s = go s [] where
  go "" acc = reverse acc
  go s acc = go (rest s) ((first s): acc)
    where rest s = dropWhile (==' ') (dropWhile (/=' ') s)
          first s = takeWhile (/=' ') s
