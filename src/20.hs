import Data.Char

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

main = print . sum . map digitToInt . show . fac $ 100000