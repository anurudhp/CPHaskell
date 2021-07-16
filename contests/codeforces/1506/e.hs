{-# LANGUAGE LambdaCase #-}

import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (group, sort)
import Data.Maybe (fromJust, fromMaybe)

main :: IO ()
main = C.interact $ runScanner (numberOf (C.pack <$> testCase)) >>> C.unlines

testCase :: Scanner String
testCase = do
  xs <- numberOf int
  return . unlines . map (unwords . map show) $ [minP xs, maxP xs]

maxP, minP :: [Int] -> [Int]
maxP = reverse . (\(ps, _, _) -> ps) . foldl nxt ([], 0, [])
  where
    nxt (ps, u, cs) v
      | u == v = let (c : cs') = cs in (c : ps, u, cs')
      | otherwise = (v : ps, v, reverse [u + 1 .. v - 1] ++ cs)

minP = reverse . (\(ps, _, _) -> ps) . foldl nxt ([], 0, newQueue)
  where
    nxt (ps, u, cs) v
      | u == v = let (c, cs') = popQ cs in (c : ps, u, cs')
      | otherwise = (v : ps, v, foldl (flip pushQ) cs [u + 1 .. v - 1])

--- Template ---
data Queue a = Queue [a] [a]

newQueue :: Queue a
newQueue = Queue [] []

pushQ :: a -> Queue a -> Queue a
pushQ x (Queue hs ts) = Queue hs (x : ts)

emptyQ :: Queue a -> Bool
emptyQ (Queue hs ts) = null hs && null ts

popQ :: Queue a -> (a, Queue a)
popQ (Queue [] ts) = let (h : hs) = reverse ts in (h, Queue hs [])
popQ (Queue (h : hs) ts) = (h, Queue hs ts)

safePopQ :: Queue a -> Queue a
safePopQ q = if emptyQ q then q else snd (popQ q)

frontQ :: Queue a -> a
frontQ = fst . popQ

safeFrontQ :: Queue a -> Maybe a
safeFrontQ q = if emptyQ q then Nothing else Just (frontQ q)

type Scanner = State [C.ByteString]

runScanner :: Scanner a -> C.ByteString -> a
runScanner = runScannerWith C.words

runScannerWith :: (C.ByteString -> [C.ByteString]) -> Scanner a -> C.ByteString -> a
runScannerWith t s = evalState s . t

peek :: Scanner C.ByteString
peek = gets head

str :: Scanner C.ByteString
str = get >>= \case s : ss -> put ss >> return s

string :: Scanner String
string = C.unpack <$> str

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
