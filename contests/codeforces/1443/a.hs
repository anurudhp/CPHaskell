import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
  words >>> drop 1 >>> map (read >>> solve >>> map show >>> unwords) >>> unlines

solve :: Int -> [Int]
solve n = [2 * (i + n) | i <- [1 .. n]]
