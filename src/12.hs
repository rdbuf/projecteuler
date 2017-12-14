import Data.List
import Data.Maybe

primes :: [Integer]
primes = 2 : 3 : 5 : primes'
  where primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])
        isPrime (p:ps) n = p ^ 2 > n || n `mod` p /= 0 && isPrime ps n
        
factorize' :: Integer -> [(Integer, Integer)]
factorize' 1 = []
factorize' n = (divisor, count) : factorize' (n `div` (divisor ^ count))
  where
    divisor = fromJust (find (\prime -> n `mod` prime == 0) primes)
    count = (last . takeWhile (\power -> n `mod` (divisor ^ power) == 0)) [1..]
    n' = fromInteger n :: Rational



main = print $ (product . map (\(a, b) -> a ^ b)) res
-- main = print res
  where triangularNumbers = scanl (+) 1 [2..]
        res = fromJust $ find (\x -> ((product . map ((+1) . snd)) x) > 500) (map factorize' triangularNumbers)