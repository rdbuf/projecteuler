import Data.List
import Data.List.Ordered

primes = naiveSieve [2..] where
    naiveSieve (x:xs) = x:naiveSieve (filter ((/= 0) . (`mod` x)) xs)

condition n = null $ (reverse . takeWhile (> 0) . map (\x -> n - 2 * x^2)) [1..] `isect` primes

main = print $ find condition ([3,5..] `minus` primes)