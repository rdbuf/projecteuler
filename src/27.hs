-- projecteuler 27

-- runs for 24 minutes total, too slow

import Data.Function
import Data.List
import Data.Maybe
import Debug.Trace

data QuadraticPolynom = Coeffs Int Int deriving Show

coeffProd (Coeffs b c) = b * c

evaluate (Coeffs b c) x = x^2 + b*x + c

primes = sieveOfEratothenes [2..] where
    sieveOfEratothenes (x:xs) = x : sieveOfEratothenes [num | num <- xs, num `mod` x /= 0]

isPrime n = n > 1 && null [p | p <- takeWhile (< n) primes, n `mod` p == 0]

consecutivePrimes polynom = length . takeWhile (isPrime . evaluate polynom) $ [0..]

main = print . coeffProd . maximumBy (compare `on` consecutivePrimes) $ Coeffs <$> [-999..999] <*> [-1000..1000]