import Control.Arrow ((>>>))
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

main :: IO ()
main =
  C.interact $
  C.lines >>>
  drop 1 >>>
  map (C.words >>> map readInteger) >>>
  process >>> map (show >>> C.pack) >>> C.unlines
  where
    readInt = C.readInt >>> fromMaybe undefined >>> fst
    readInteger = readInt >>> fromIntegral

process :: [[Integer]] -> [Integer]
process [] = []
process (_:as:bs:xss) = solve as bs : process xss

solve :: [Integer] -> [Integer] -> Integer
solve as bs = minimum costs
  where
    (as', bs') = unzip . sortBy (comparing fst) $ zip as bs
    costs = zipWith max (scanl max 0 as') (scanr (+) 0 bs')
