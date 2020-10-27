{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))
import safe Data.List (sort)

main :: IO ()
main =
  interact $
    lines
      >>> drop 1
      >>> map (words >>> map read)
      >>> process
      >>> map show
      >>> unlines

process :: [[Int]] -> [Integer]
process [] = []
process ([_, k] : xs : xss) = solve k xs : process xss

solve :: Int -> [Int] -> Integer
solve k = sum . map toInteger . take (k + 1) . reverse . sort
