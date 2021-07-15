{-# LANGUAGE LambdaCase #-}

import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (group)
import Data.Maybe (fromJust, fromMaybe)

chunksOf _ [] = []
chunksOf k xs = let (hs, ts) = splitAt k xs in hs : chunksOf k ts

main :: IO ()
main = C.interact $ runScanner (numberOf testCase) >>> C.unlines

testCase :: Scanner C.ByteString
testCase = do
  n <- int
  xs <- n >< int
  return . C.pack . show $ solve n xs

solve n xs = 2 * n - 1 + compute xs
  where
    compute [] = 0
    compute [_] = 0
    compute [_, _] = 0
    compute (x : y : z : xs) = good3 x y z + (case xs of [] -> 0; (w:_) -> good4 x y z w) + compute (y : z : xs)

    good p q r = q < min p r || q > max p r
    good3 p q r | good p q r = 1 | otherwise = 0
    good4 p q r s | good p q r && good p r s && good p q s && good q r s = 1
      | otherwise = 0

--- Template ---
type Scanner = State [C.ByteString]

runScanner :: Scanner a -> C.ByteString -> a
runScanner = runScannerWith C.words

runScannerWith :: (C.ByteString -> [C.ByteString]) -> Scanner a -> C.ByteString -> a
runScannerWith t s = evalState s . t

peek :: Scanner C.ByteString
peek = gets head

str :: Scanner C.ByteString
str = get >>= \case s : ss -> put ss >> return s

int :: Scanner Int
int = fst . fromJust . C.readInt <$> str

integer :: Scanner Integer
integer = read . C.unpack <$> str

double :: Scanner Double
double = read . C.unpack <$> str

decimal :: Int -> Scanner Int
decimal p = round . ((10 ^ p) *) <$> double

numberOf :: Scanner a -> Scanner [a]
numberOf s = int >>= flip replicateM s

many :: Scanner a -> Scanner [a]
many s = get >>= \case [] -> return []; _ -> (:) <$> s <*> many s

till :: (C.ByteString -> Bool) -> Scanner a -> Scanner [a]
till p s = do
  t <- peek
  if p t then return [] else (:) <$> s <*> till p s

times :: Int -> Scanner a -> Scanner [a]
times = replicateM

(><) = times

two, three, four :: Scanner a -> Scanner [a]
[two, three, four] = map times [2 .. 4]

pair :: Scanner a -> Scanner b -> Scanner (a, b)
pair = liftA2 (,)
