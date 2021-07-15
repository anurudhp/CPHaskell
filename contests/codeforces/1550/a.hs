{-# LANGUAGE LambdaCase #-}

import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (fromJust, fromMaybe)

main :: IO ()
main = interact $ words >>> drop 1 >>> map (read >>> solve >>> show) >>> unlines

solve :: Int -> Int
solve s = (1 +) . length . takeWhile (< s) $ scanl1 (+) [1, 3 ..]

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
