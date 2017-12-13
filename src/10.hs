-- algorithm was taken from 
-- https://www.reddit.com/r/haskell/comments/35vc31/the_real_way_to_generate_a_list_of_primes_in/cr8pjz9/
primes :: [Integer]
primes = 2 : 3 : 5 : primes'
  where primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])
        isPrime (p:ps) n = p ^ 2 > n || n `mod` p /= 0 && isPrime ps n


main :: IO ()
main = print $ sum $ takeWhile (< 2000000) $ primes