import Data.List.Ordered

main = print . map postprocess . filter valid . map (\x -> [x,x+3330,x+2*3330]) $ [1000..9999-2*3330]
valid = (&&) <$> (== 1) . length . nub . sort . map (sort . show) <*> all (`member` primes)
primes = sieve [2..] where sieve (x:xs) = x:sieve (filter (\y -> y `mod` x /= 0) xs)
postprocess = (read :: String -> Int) . concat . map show