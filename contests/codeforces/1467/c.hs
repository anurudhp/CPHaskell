import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $ lines >>> drop 1 >>> map (words >>> map read) >>> solve >>> show

solve :: [[Integer]] -> Integer
solve xss = sum (map sum xss) - 2 * maximum [sum ms - maximum ms]
  where
    ms = map minimum xss
    f = filter ((== 2) . length) xss
