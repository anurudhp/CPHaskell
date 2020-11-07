import Control.Arrow ((>>>))
import Numeric (showFFloat)

main :: IO ()
main =
  interact $
  lines >>>
  drop 1 >>>
  map (words >>> map read) >>> solve >>> showFFloat (Just 10) >>> ($ "\n")

solve :: [[Double]] -> Double
solve = error ""
