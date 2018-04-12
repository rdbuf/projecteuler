import Data.Char
import Numeric
import Data.Monoid
import Data.Function

main = print . sum . filter valid $ [1..pred 1000000]
valid = ((&&) `on` isPalindrome) <$> show <*> ($ "") . showIntAtBase 2 intToDigit
isPalindrome = ((==) `on` take =<< (`div` 2) . length) =<< reverse