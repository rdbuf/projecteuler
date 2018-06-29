import Control.Monad
import Data.Char

main = print . product . map digitToInt $ map ($ join . map show $ [1..]) [(!!9), (!!9), (!!99), (!!999), (!!9999), (!!99999), (!!999999)]