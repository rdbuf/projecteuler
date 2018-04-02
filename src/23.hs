-- projecteuler 23

import Data.List
import Control.Monad

divisors n = [x | x <- [1..n `div` 2], n `mod` x == 0]

isAbundant n = sum (divisors n) > n

abundant from to = [x | x <- [from..to], isAbundant x]

bound = 28123 :: Integer

pairwiseSums = map head . group . sort . join . map generate . takeWhile (not . null) . iterate tail $ abundant 1 bound
generate list = takeWhile (<= bound) . map (head list +) $ list

main = print $ sum [1..bound] - sum pairwiseSums
