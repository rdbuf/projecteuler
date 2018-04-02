-- projecteuler 24

import Data.Char
import Data.List

main = putStr . map intToDigit . (!! 999999) . sort $ permutations [0..9]