import Control.Arrow ((>>>))

main :: IO ()
main = interact $ read >>> solve >>> show

solve :: Int -> Double
solve n = sum [fromIntegral n / fromIntegral i | i <- [1 .. n - 1]]
