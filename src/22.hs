import Text.Parsec hiding (token)
import Data.List
import Control.Monad
import Data.Char

grammar = (char '"' *> many1 letter <* char '"') `sepBy` char ','

main = print . fmap process . parse grammar "" =<< getContents where
    process = sum . zipWith (*) [1..] . map (sum . map (toInteger . alphabetPos . toLower)) . sort where
        alphabetPos c = ord c - ord 'a' + 1
