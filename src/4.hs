import Data.List

main :: IO ()
main = print $ last $ sort $ filter (\x -> (show x) == (reverse $ show x)) [a * b | a <- reverse [100..999], b <- reverse [100..999]]