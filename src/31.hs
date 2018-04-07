-- Bruteforce / 1 hour

-- coins = [1,2,5,10,20,50,100,200]
-- target = 200
-- maxAmounts = map (target `div`) coins

-- main = print . length . filter ((== target) . sum) . sequence $ zipWith map (map (*) coins) (map (enumFromTo 0) maxAmounts)


-- Bruteforce optimized by a constant factor / 16 seconds

-- coins = [2,5,10,20,50,100,200]
-- target = 200
-- maxAmounts = map (target `div`) coins

-- main = print . length . filter ((<= target) . sum) . sequence $ zipWith map (map (*) coins) (map (enumFromTo 0) maxAmounts)


-- Recursive formula / 0 seconds

import qualified Data.Map as M
import Data.List

coins = reverse [1,2,5,10,20,50,100,200]
ways (coin:coins) target | coin == 1 || target == 0 = 1
                         | otherwise = sum [ways coins (target - coin * i) | i <- [0..target `div` coin]]
main = print $ ways coins 200
