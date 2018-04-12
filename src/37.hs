import Data.Monoid
import Data.List
import Data.Function

primes = [2,3,5,7] <> filter (isPrime primes) (scanl (+) 11 (cycle [2,4,2,4,6,2,6,4]))
isPrime (x:xs) n = n >= 2 && (x ^ 2 > n || n `mod` x /= 0 && isPrime xs n)

main = print . sum . take 11 . filter valid . drop 4 $ primes
valid = all (isPrime primes . read) . (((<>) `on` init . tail) <$> inits <*> tails) . show