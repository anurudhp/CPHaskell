import Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> drop 1 >>> map (read >>> solve >>> show) >>> unlines

solve :: Integer -> Integer
solve n =
  (`mod` (10 ^ 9 + 7))
    . (n +)
    . sum
    . map (n `div`)
    . takeWhile (<= n)
    $ scanl1 lcm [1 ..]
