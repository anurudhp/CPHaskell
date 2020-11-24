import Control.Arrow ((>>>))
import Data.Bool (bool)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (fromMaybe, isJust)

main :: IO ()
main =
  interact $ lines >>> drop 1 >>> process >>> map (bool "No" "Yes") >>> unlines
  where
    readInt = C.readInt >>> fromMaybe undefined >>> fst

process :: [String] -> [Bool]
process [] = []
process (nk:a:b:xs) = solve k a b : process xs
  where
    k = words >>> last >>> read $ nk

solve :: Int -> String -> String -> Bool
solve k a b = (== Just 0) . foldl f (Just 0) $ zipWith (-) (freq a) (freq b)
  where
    freq s = [length (filter (== c) s) | c <- ['a' .. 'z']]
    f Nothing _ = Nothing
    f (Just acc) x
      | acc + x < 0 = Nothing
      | (acc + x) `mod` k /= 0 = Nothing
      | otherwise = Just (acc + x)
