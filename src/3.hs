import Data.List
import Data.Maybe

main :: IO ()
main = print $ (last . filter (\x -> targetNumber `mod` x == 0 && isPrime x)) (2 : [3, 5..bound])
  where bound = (floor . sqrt . fromInteger) targetNumber :: Integer
        targetNumber = 600851475143
        isPrime n = (isJust . find (\x -> n `mod` x == 0)) (2 : [3, 5..bound])
          where bound = (floor . sqrt . fromInteger) n :: Integer