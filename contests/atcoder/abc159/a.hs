-- AC https://atcoder.jp/contests/abc159/submissions/11095079

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    words >>> map (read :: String -> Int) >>> solve >>> show

solve :: [Int] -> Int
solve xs = foldl1 (+) $ (\x -> div (x * (x - 1)) 2) <$> xs
