import Data.List
import System.Environment
import System.Exit

main = getArgs >>= parse >>= putStr

parse ["-h"] = usage >> exitSuccess
parse [fname] = unlines . extract . lines <$> readFile fname
parse _ = die "invalid usage"

usage =
  putStrLn $
  intercalate
    "\n"
    [ "Usage: runhaskell getYAMLMetadata <path-to-file>"
    , "Extracts YAML metadata from file. Only works if first line is `---`"
    ]

extract :: [String] -> [String]
extract ("---":xs) = (++ ["---"]) . ("---" :) . takeWhile (/= "---") $ xs
extract xs = xs
