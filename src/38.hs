-- at least two components; in this extremal case, it's 4 and 5 digits

import Data.List
import Data.Maybe

countDigits n = fromJust . find ((n <) . (10^)) $ [1..]

isPandigital = ((&&) <$> (== 9) . length <*> (== ['1'..'9']) . sort) . show

-- for [1..9999], try to construct a pandigital and maintain the maximum
main = print . maximum . filter isPandigital $ [ number | seed <- [1..9876], let (Just number) = find ((>= 9) . countDigits) . scanl1 (\l r -> l * 10^countDigits r + r) . map (* seed) $ [1..]]