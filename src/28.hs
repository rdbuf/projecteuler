-- projecteuler 28

-- The other solution by @polkovnikov-ph:
-- main = print . sum . scanl (+) 1 . (>>= replicate 4) $ [2,4..1001]

import Data.Monoid

lengths = replicate 2 =<< [1..]
values = [2..]

spiral = zipWith take lengths beginnings where
    beginnings = values : zipWith drop lengths beginnings

diagonalNumbers = (1 :) . zipWith ($) (cycle $ replicate 3 last <> [last . init]) . drop 1

main = print . sum $ diagonalNumbers (take (1001 `div` 2 * 4 + 1) spiral)
