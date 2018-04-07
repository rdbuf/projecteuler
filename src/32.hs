-- projecteuler 32

-- The idea:
--
-- let ln = logBase 10 in
-- ln A `elem` [ln B + ln C, ln B + ln C + 1]
--   && ln A + ln B + ln C `elem` [6..8]
--   && ln A + ln B + ln C + 3 = 9
-- => ln A + ln B + ln C = 6
-- => ln A = ln B + ln C = 3
-- => [(3, [(0, 3), (1, 2)])]


-- Naive approach / 16 seconds

-- import Data.Function
-- import Data.Maybe
-- import Control.Monad
-- import Data.List hiding (permutations)

-- chunksOf n = map (take n) . takeWhile (not . null) . iterate (drop n)

-- permutations [] = [[]]
-- permutations list = join [map (x:) $ permutations (x `delete` list) | x <- list]


-- valid permutation = read left `elem` [read a * read b | i <- [1..2], let (a, b) = splitAt i right] where
--     (left, right) = splitAt 4 permutation

-- main = print . sum . map (read . take 4 . fromJust) . filter isJust . map (find valid) . chunksOf 120 . permutations $ ['1'..'9']


-- Let's generate only the operands (2 x 15120) and see whether it gives us a pandigital product. / 0 seconds

import Data.Function
import Data.Maybe
import Control.Monad
import Data.List hiding (permutations)
import Data.Monoid

-- Lexicographically ordered on sorted input
permutationsN 0 _ = [[]]
permutationsN n list = join [map (x:) $ permutationsN (pred n) (x `delete` list) | x <- list]

process permutation = [if isPandigital $ permutation <> show (read a * read b) then Just $ read a * read b else Nothing 
                       | i <- [1..2], let (a, b) = splitAt i permutation]

isPandigital = (== ['1'..'9']) . sort

main = print . sum . nub . map fromJust . filter isJust . join . map process . permutationsN 5 $ ['1'..'9']