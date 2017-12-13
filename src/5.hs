import Data.Ratio
import Data.List

sieveOfEratothenes :: Integer -> [Integer]
sieveOfEratothenes bound = f [2..bound]
  where 
    f :: [Integer] -> [Integer]
    f (prime:rest) = [prime] ++ f (filter (\num -> num `mod` prime /= 0) rest)
    f [] = []

primes = sieveOfEratothenes 20


factorize' :: Integer -> [(Integer, Integer)]
factorize' 1 = []
factorize' n = [(divisor, count)] ++ factorize' (numerator $ n' / (divisor' ^^ count))
  where
    divisor = case find (\prime -> n `mod` prime == 0) primes
      of
        Just m -> m
        Nothing -> n
    count = last $ takeWhile (\power -> n `mod` numerator (divisor' ^^ power) == 0) [1..]
    n' = fromInteger n :: Rational
    divisor' = fromInteger divisor :: Rational
    
    
main :: IO ()
main = print result
  where
    allDivisors = (reverse . sort . concat) $ map factorize' [1..20]
    filteredDivisors = foldl (\x y -> x ++ if ((fst . last) x == fst y) then [] else [y])  [(1,0)] allDivisors
    result = foldl (\res (prime, power) -> res * numerator ((fromInteger prime :: Rational) ^^ power)) 1 filteredDivisors