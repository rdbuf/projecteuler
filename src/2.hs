main :: IO ()
main = print . sum $ filter (\x -> even x) $ takeWhile (\x -> x <= 4000000) [fib(n) | n <- [1, 2 ..]]
  where 
    fib 1 = 1
    fib 2 = 2
    fib n = fib (n - 1) + fib (n - 2)