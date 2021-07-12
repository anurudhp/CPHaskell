import Control.Arrow ((>>>))
import Data.Bool (bool)
import Data.Char (chr, ord)

main :: IO ()
main = interact $ lines >>> drop 1 >>> map (isAlphabetic >>> bool "No" "Yes") >>> unlines

isAlphabetic :: String -> Bool
isAlphabetic "" = True
isAlphabetic s
  | head s == c = isAlphabetic $ tail s
  | last s == c = isAlphabetic $ init s
  | otherwise = False
  where
    c = chr $ ord 'a' + length s - 1
