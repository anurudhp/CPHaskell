{-# LANGUAGE LambdaCase #-}

import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (sort)
import Data.Maybe (fromMaybe, fromJust)

-- https://github.com/byorgey/comprog-hs/blob/master/ScannerBS.hs
type Scanner = State [C.ByteString]

runScanner :: Scanner a -> C.ByteString -> a
runScanner = runScannerWith C.words

runScannerWith :: (C.ByteString -> [C.ByteString]) -> Scanner a -> C.ByteString -> a
runScannerWith t s = evalState s . t

str :: Scanner C.ByteString
str = get >>= \case s : ss -> put ss >> return s

int :: Scanner Int
int = fst . fromJust . C.readInt <$> str

numberOf :: Scanner a -> Scanner [a]
numberOf s = int >>= flip replicateM s

pair :: Scanner a -> Scanner b -> Scanner (a, b)
pair = liftA2 (,)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = let (hs, ts) = splitAt k xs in hs : chunksOf k ts

main :: IO ()
main = C.interact $ runScanner (numberOf (C.pack . unwords . map show <$> testCase)) >>> C.unlines

testCase :: Scanner [Int]
testCase = do
  (n, k) <- pair int int
  as <- replicateM k int
  ts <- replicateM k int
  return $ solve [[n, k], as, ts]

solve :: [[Int]] -> [Int]
solve [[n, k], as, ts] = zipWith min (compute xs) (reverse . compute . reverse $ xs)
  where
    xs = tail . snd . foldr upd (n + 1, []) . sort $ ((0, 0) : zip as ts)
    upd (a, t) (i, xs) = (a, t : replicate (i - a - 1) inf ++ xs)
    inf = 2 * 10 ^ 9

compute :: [Int] -> [Int]
compute = scanl1 (\p x -> min (p + 1) x)
