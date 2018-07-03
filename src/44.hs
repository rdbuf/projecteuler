import Data.List
import Data.List.Ordered
import Data.Maybe

pentagonal k = k * (3 * k - 1) `div` 2

pentagonals = map pentagonal [1..]

-- This solution surprizingly worked, but it's unfair; we need to prove the uniqueness of the said pair or something.
data Solution = Solution { c :: Int, d :: Int, value :: Int } deriving Show
main = print [Solution c d (pentagonal d - pentagonal c) |
    s <- [1..], c <- [1..s `div` 2], let d = s - c, (pentagonal c + pentagonal d) `member` pentagonals, (pentagonal d - pentagonal c) `member` pentagonals]