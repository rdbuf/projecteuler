import Data.List
import Data.Ratio
import Data.Maybe

main = print (a * b * c)
  where targetSum = 1000
        pythagoreanTriplets = [(a, b, fromJust (maybeC a b)) | a <- [1..targetSum], 
                                                        b <- [a..targetSum], 
                                                        isJust (maybeC a b)]
          where maybeC a b | c ^ 2 == a ^ 2 + b ^ 2 = Just c 
                           | otherwise = Nothing
                             where c = (floor . sqrt . fromIntegral) (a ^ 2 + b ^ 2)
        (a, b, c) = fromJust $ find (\(a, b, c) -> a + b + c == targetSum) pythagoreanTriplets