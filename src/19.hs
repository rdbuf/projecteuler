-- projecteuler 19

import Control.Monad
import Data.Monoid

isLeap n = n `mod` 4 == 0 && (n `mod` 100 /= 0 || n `mod` 400 == 0)

daysInMonth year = [31, 28 + fromEnum (isLeap year), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

data Day = Day { dayOfWeek :: Int, dayOfMonth :: Int }

main = print . length . filter valid . zipWith Day ([1 + sum (daysInMonth 1900) `mod` 7..7] <> (join . repeat) [1..7]) $ enumFromTo 1 =<< daysInMonth =<< [1901..2000]

valid (Day dow dom) = dow == 7 && dom == 1
