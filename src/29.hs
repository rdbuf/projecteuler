import Data.List
main = print . length . nub . sort $ (^) <$> [2..100] <*> [2..100]