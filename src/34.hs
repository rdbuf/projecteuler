-- projecteuler 34

import Data.Char

fac = product . enumFromTo 1

main = putStr . unlines . map show . scanl1 (+) . filter ((==) =<< sum . map (fac . digitToInt) . show) $ [10..]
