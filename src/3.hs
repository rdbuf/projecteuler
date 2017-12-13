import Data.List

targetNumber = 600851475143

-- sieveOfEratosthenes :: Integer -> [Integer]
-- sieveOfEratosthenes bound = f [2,3..bound]
--   where
--     f :: [Integer] -> [Integer]
--     f (prime:rest) = [prime] ++ (f $ filter (\num -> num `mod` prime /= 0) rest)
--     f [] = []

isPrime n = case find (\x -> n `mod` x == 0) $ [2] ++ [3, 5..bound] of 
              Nothing -> True 
              otherwise -> False
  where
    bound = floor $ sqrt $ fromInteger n :: Integer
    
main :: IO ()
main = print $ last $ filter (\x -> targetNumber `mod` x == 0 && isPrime x) $ [2] ++ [3, 5..bound]
  where
    bound = floor $ sqrt $ fromInteger targetNumber :: Integer