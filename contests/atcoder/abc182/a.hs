import Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> map read >>> solve >>> show

solve :: [Int] -> Int
solve [a, b] = 2 * a + 100 - b
