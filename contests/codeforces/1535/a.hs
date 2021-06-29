import Control.Arrow ((>>>))
import Data.List(sort)
import Data.Bool(bool)

main :: IO ()
main = interact $ lines >>> drop 1
         >>> map (words >>> map read >>> solve >>> bool "NO" "YES")
         >>> unlines

solve :: [Int] -> Bool
solve ss@[s1, s2, s3, s4] = max s1 s2 + max s3 s4 == (sum . drop 2 . sort $ ss)
