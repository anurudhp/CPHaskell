import Control.Arrow ((>>>))
import Data.Char (toUpper)

main :: IO ()
main = interact $ lines >>> solve >>> (++ "\n")

solve :: [String] -> String
solve ["Y", t] = toUpper <$> t
solve ["N", t] = t
