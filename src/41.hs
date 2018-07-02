import Data.List hiding (permutations)
import Data.Char
import Data.Monoid

-- Naive yet enough.
isPrime n = all (/= 0) . map (n `mod`) $ 2:[3, 5..n `div` 2]

-- Generates permutations in lexical order.
permutations [single] = [[single]]
permutations multiple = concat [map (head right :) (permutations (left <> tail right)) | (left, right) <- map (($ multiple) . splitAt) [0..length multiple-1]]

pandigitals = [read . map intToDigit $ perm | i <- [9,8..1], perm <- permutations [i, i-1..1]]

main = print . find isPrime $ pandigitals