import Data.Char
main = putStr . unlines . map show . scanl1 (+) . filter ((==) =<< sum . map ((^5) . digitToInt) . show) $ [10..]