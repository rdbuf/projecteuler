import Data.Char
import Data.List.Ordered
import Data.List.Split

triangle n = n * (n + 1) `div` 2
triangles = map triangle [1..]

evaluate c = fromEnum c - fromEnum 'A' + 1

main = do
    contents <- map (read :: String -> String) . splitOn "," <$> getContents
    let sums = map (sum . map evaluate) $ contents
    print . length . filter (($ triangles) . member) $ sums