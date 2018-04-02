import Data.Monoid
import Data.List

fib :: [Integer]
fib = [1, 1] <> zipWith (+) fib (drop 1 fib)

main = print . fmap (+1) . findIndex ((>= 1000) . length . show) $ fib