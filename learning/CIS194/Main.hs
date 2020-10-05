module Main where

import System.IO (hFlush, stdout)
import qualified Week5
import qualified Week6
import qualified Week7

main :: IO ()
main = do
  putStr "Assignment to test (week-no): "
  hFlush stdout
  n <- (read :: String -> Int) <$> getLine
  case n of
    5 -> Week5.main
    6 -> Week6.main
    7 -> Week7.main
    _ -> putStrLn "No such assignment found"
