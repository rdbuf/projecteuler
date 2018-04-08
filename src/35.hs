-- projecteuler 35

import Data.Monoid
import Data.Char

primes = [2, 3, 5, 7] <> filter (checkPrimality primes) (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])
checkPrimality (p:rest) n = n < p ^ 2 || n `mod` p /= 0 && checkPrimality rest n -- do not fully get this code (sic!)

isPrime = checkPrimality primes

rotations lst = take (length lst) . iterate ((<>) <$> drop 1 <*> take 1) $ lst

diagnostics = [
        (||) <$> (not . any ((== 0) . (`mod` 2) . digitToInt) . show) <*> (== 2),
        all (isPrime . read) . rotations . show
    ]

main = print . length . filter (\number -> all ($ number) $ diagnostics) . takeWhile (<= 1000000) $ primes
