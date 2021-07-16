{-# LANGUAGE LambdaCase #-}

import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (fromJust, fromMaybe)

main :: IO ()
main = C.interact $ runScanner (numberOf (C.pack <$> testCase)) >>> C.unlines

testCase :: Scanner String
testCase = do
  (n, k) <- pair int int
  s <- str
  return . show $ compute k s

compute :: Int -> String -> Int
compute k s = replace (-10^9) [i | (c, i) <- zip s [1..], c == '*']
  where
    replace _ [] = 0
    replace _ [_] = 1
    replace p (x:x':xs)
      | x' <= p + k = replace p (x':xs)
      | otherwise = 1 + replace x (x':xs)

--- Template ---
type Scanner = State [C.ByteString]

runScanner :: Scanner a -> C.ByteString -> a
runScanner = runScannerWith C.words

runScannerWith :: (C.ByteString -> [C.ByteString]) -> Scanner a -> C.ByteString -> a
runScannerWith t s = evalState s . t

peek :: Scanner C.ByteString
peek = gets head

cstr :: Scanner C.ByteString
cstr = get >>= \case s : ss -> put ss >> return s

str :: Scanner String
str = C.unpack <$> cstr

int :: Scanner Int
int = fst . fromJust . C.readInt <$> cstr

integer :: Scanner Integer
integer = read . C.unpack <$> cstr

double :: Scanner Double
double = read . C.unpack <$> cstr

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

