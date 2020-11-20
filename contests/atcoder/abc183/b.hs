import Control.Arrow ((>>>))
import Numeric (showFFloat)

main :: IO ()
main =
  interact $ words >>> map read >>> solve >>> showFFloat (Just 10) >>> ($ "\n")

solve :: [Double] -> Double
solve [sx, sy, gx, gy] = sx + sy * (gx - sx) / (gy + sy)
