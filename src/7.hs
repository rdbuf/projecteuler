sieveOfEratothenes :: Int -> [Int]
sieveOfEratothenes bound = f [2..bound]
  where f (prime:rest) = prime : (f . filter (\num -> num `mod` prime /= 0)) rest
        f [] = []

main = print result
  where n = 10001 :: Int
        result = (last . take n) (sieveOfEratothenes bound)  
                    -- remove `last` to see the primes as they get generated
        bound = n * n
