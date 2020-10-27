import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $ 
    lines 
      >>> drop 1 
      >>> map (words >>> map read >>> solve >>> show) 
      >>> unlines

solve :: [Int] -> Int
solve [a, b]
  | a `mod` b == 0 = 0
  | otherwise = b - (a `mod` b)

