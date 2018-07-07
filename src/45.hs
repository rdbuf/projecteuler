import Data.List
import Data.List.Ordered

triangle = map (\n -> n*(n+1) `div` 2) [286..] :: [Int]
pentagonal = map (\n -> n*(3*n-1) `div` 2) [166..] :: [Int]
hexagonal = map (\n -> n*(2*n-1)) [144..] :: [Int]

main = print $ find ((&&) <$> has triangle <*> has pentagonal) hexagonal