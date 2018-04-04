-- projecteuler 26

-- idea borrowed from https://math.stackexchange.com/questions/377683/length-of-period-of-decimal-expansion-of-a-fraction

import Data.Ratio
import Data.Monoid
import Data.List
import Data.Function
import Data.Maybe

periodLength :: Rational -> Int
periodLength n | denominator n `mod` 2 == 0 || denominator n `mod` 5 == 0 = 0
               | otherwise = fromJust $ find ((== 1) . (`mod` denominator n) . (10 ^)) [1..]

main = print $ maximumBy (compare `on` periodLength . (1 %)) [2..1000]