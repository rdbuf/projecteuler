-- projecteuler 33

import Data.List
import Data.Monoid
import Data.Ratio

main = print . denominator . product $ 
    [a % b | a <- [10..98], b <- [a+1..99], 
             a `mod` 10 /= 0, b `mod` 10 /= 0, b `mod` 11 /= 0,
             any id [read (digit `delete` show a) % read (digit `delete` show b) == a % b | digit <- show a `intersect` show b]]