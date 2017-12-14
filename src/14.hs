{-# LANGUAGE MultiWayIf #-}

-- Naive solution, quite slow

collatz :: Integer -> [Integer]
collatz n = n : if | n == 1 -> []
                   | n `mod` 2 == 0 -> collatz (n `div` 2)
                   | otherwise -> collatz (n * 3 + 1)
  
main = (print . maximum) candidates
  where candidates = map (\n -> ((length . collatz) n, n)) [1..1000000-1]
