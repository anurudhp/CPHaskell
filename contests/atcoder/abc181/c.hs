import Control.Arrow ((>>>))
import Data.Bool (bool)

main :: IO ()
main =
  interact $
  lines >>> drop 1 >>> map (words >>> map read) >>> solve >>> bool "No" "Yes"

solve :: [[Int]] -> Bool
solve pts = any ((== 0) . area) triples
  where
    triples =
      [ (p1, p2, p3)
      | p1 <- pts
      , p2 <- pts
      , p2 /= p1
      , p3 <- pts
      , p3 /= p1
      , p3 /= p2
      ]

type Point = [Int]

area :: (Point, Point, Point) -> Int
area (p1, p2, p3) = det p1 p2 + det p2 p3 + det p3 p1
  where
    det [x1, y1] [x2, y2] = x1 * y2 - x2 * y1
{- determinant of:
1  1  1
x1 x2 x3
y1 y2 y3
-}
