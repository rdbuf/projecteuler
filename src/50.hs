{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Function
import Data.List
import Data.Numbers.Primes
import qualified Data.IntSet as IS
import Numeric.Module.Class
import Numeric.Additive.Class
import Prelude hiding ((+))

data Result = Result { value :: Int, len :: Int } deriving (Show)
instance Additive Result where (+) = undefined -- this property is unused
instance RightModule Int Result where (Result v l) *. p = Result (v + p) (l + 1)

main =
    print .
    maximumBy (compare `on` len) .
    map last .
    takeWhile (not . null) .
    map (
        filter ((`IS.member` targetPrimes) . value) .
        takeWhile ((< 1000000) . value) .
        scanl (*.) (Result 0 0)
    ) .
    tails $
        (primes :: [Int])
targetPrimes = IS.fromDistinctAscList . takeWhile (< 1000000) $ primes :: IS.IntSet