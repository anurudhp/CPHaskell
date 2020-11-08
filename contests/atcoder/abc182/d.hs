import Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> drop 1 >>> map read >>> solve >>> show

solve :: [Int] -> Int
solve xs = maximum $ zipWith (+) (0 : ppre) (mxs ++ [0])
  where
    pre = scanl1 (+) xs
    mxs = tail $ scanl max 0 pre
    ppre = scanl1 (+) pre
