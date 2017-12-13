import Prelude
import Data.Ratio

sumAriphmSequence :: Integer -> Integer -> Integer -> Integer
sumAriphmSequence a d max = numerator $ a' * (n' + 1) + (n' + 1) * n' / 2 * d' 
                                   -- = a' + a' + d' + a' + 2d' + ... + a' + nd'
                                   -- = a' * (n' + 1) + (n' + 1) * n' / 2 * d'
  where
    a' = fromInteger a :: Rational
    d' = fromInteger d :: Rational
    n' = fromInteger ((max - a) `div` d) :: Rational
    
sumAriphmSequence' :: Integer -> Integer -> Integer -> Integer
sumAriphmSequence' a d max = sum $ takeWhile (\x -> x <= max) [a, a + d ..]  -- this doesn't get optimized

sumThirds = sumAriphmSequence a d max
  where
    a = 3
    d = a
    max = 9999999
    
sumFifths = sumAriphmSequence a d max
  where
    a = 5
    d = a
    max = 9999999

sumFifteenths = sumAriphmSequence a d max
  where
    a = 3 * 5
    d = a
    max = 9999999

main :: IO ()
main = print $ sumThirds + sumFifths - sumFifteenths