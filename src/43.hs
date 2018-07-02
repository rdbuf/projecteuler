import Data.List
import Data.Char

-- It would be more interesting to use the properties of divisibility.

condition n = all ($ n) (zipWith (\d off -> (== 0) . (`mod` d) . read . map intToDigit . take 3 . drop off) divisors substringOffsets) where
    divisors = [2, 3, 5, 7, 11, 13, 17]
    substringOffsets = [1..7]

main = print . sum . map (read . map intToDigit) . filter condition . permutations $ [0..9]