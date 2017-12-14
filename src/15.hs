main = (print . last . last . take n) res
  where (n, m) = (21, 21)
        res = ((take m . repeat) 1) : map (\previous -> foldl (\x y -> x ++ [last x + y]) [1] (tail previous)) res