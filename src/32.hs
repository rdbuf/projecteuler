-- projecteuler 32

-- let ln = logBase 10 in
-- ln A `elem` [ln B + ln C, ln B + ln C + 1]
--   && ln A + ln B + ln C `elem` [6..8]
--   && ln A + ln B + ln C + 3 = 9
-- => ln A + ln B + ln C = 6
-- => ln A = ln B + ln C = 3
-- => [(3, [(0, 3), (1, 2), (2, 1), (3, 0)])]

import Data.Function
import Data.Maybe
import Control.Monad
import Data.List hiding (permutations)

chunksOf n = map (take n) . takeWhile (not . null) . iterate (drop n)

-- Lexicographically ordered on sorted input
permutations [] = [[]]
permutations list = join [map (x:) $ permutations (x `delete` list) | x <- list]

valid permutation = read left `elem` [read a * read b | i <- [1..4], let (a, b) = splitAt i right] where
    (left, right) = splitAt 4 permutation

main = print . sum . map (read . take 4 . fromJust) . filter isJust . map (find valid) . chunksOf 120 . permutations $ ['1'..'9']