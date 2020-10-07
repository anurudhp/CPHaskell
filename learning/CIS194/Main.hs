module Main where

import System.Environment (getArgs)
import qualified Week5
import qualified Week6
import qualified Week7
import qualified Week8

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Usage: run <assignment-id> [args...]"
    else
      let (n : args') = args
       in case n of
            "5" -> Week5.main args'
            "6" -> Week6.main args'
            "7" -> Week7.main args'
            "8" -> Week8.main args'
            _ -> putStrLn "No such assignment found"
