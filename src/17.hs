{-# LANGUAGE MultiWayIf #-}

import Data.Char

-- Dirty but does its work

speak :: Int -> String
speak n = case n of
              1 -> "one"
              2 -> "two"
              3 -> "three"
              4 -> "four"
              5 -> "five"
              6 -> "six"
              7 -> "seven"
              8 -> "eight"
              9 -> "nine"
              10 -> "ten"
              11 -> "eleven"
              12 -> "twelve"
              13 -> "thirteen"
              14 -> "fourteen"
              15 -> "fifteen"
              16 -> "sixteen"
              17 -> "seventeen"
              18 -> "eighteen"
              19 -> "nineteen"
              20 -> "twenty"
              30 -> "thirty"
              40 -> "forty"
              50 -> "fifty"
              60 -> "sixty"
              70 -> "seventy"
              80 -> "eighty"
              90 -> "ninety"
              1000 -> "one thousand"
              otherwise -> hundreds ++ and' ++ dozens ++ dash ++ units
                where hundreds = (if n >= 100 
                                  then (speak (n `div` 100)) ++ " hundred" 
                                  else "")
                      dozens = if (n `mod` 100 > 19) 
                               then speak (n `mod` 100 `div` 10 * 10)
                               else ""
                      units = if | (n `mod` 100 > 19) -> speak (n `mod` 10)
                                 | (n `mod` 100 == 0) -> ""
                                 | Prelude.otherwise -> speak (n `mod` 100)
                      and' = if | ((not . null) hundreds) && ((or . map (not . null)) [dozens, units]) -> " and "
                                | Prelude.otherwise -> ""
                      dash = if (and . map (not . null)) [dozens, units]
                             then "-"
                             else ""
                             
                             
main = (print . sum . map (length . filter (isAlpha) . speak)) [1..1000]

