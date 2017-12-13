main = print (squaredSum - sumSquares)
  where numbers = [1..100] :: [Rational]
        squaredSum = sum numbers ^^ 2
        sumSquares = (sum . map (^^2)) numbers