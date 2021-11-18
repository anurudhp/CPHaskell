{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Monad
import Control.Monad.ST
import Control.Monad.State (State, evalState, get, gets, put)
import qualified Data.Array as A
import Data.Array.ST
import qualified Data.Array.Unboxed as U
import Data.Bool
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Function
import Data.Int
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple (swap)

main :: IO ()
main = C.interact $ runScanner input >>> solve

data TC = TC {n :: Int, items :: [String], sus :: [(String, String)]}

input :: Scanner TC
input = do
  n <- int
  items <- n >< str
  sus <- numberOf (pair str str)
  return TC {..}

-- solve :: TC -> Output
solve TC {..} = C.pack $ runST $ do
  col <- newArray (1, n) (-1)
  ok <- and <$> forM [1..n] (dfs col (-1))
  if not ok
     then return "impossible"
     else do
      col <- getElems col
      return $ unlines $ getItems col <$> [0, 1]
  where
    itemIDs :: M.Map String Int
    itemIDs = M.fromList $ zip items [1 ..]
    getID = fromJust . (`M.lookup` itemIDs)

    getItems cols c = unwords [it | (it, c') <- zip items cols, c == c']

    edges = [(getID itu, getID itv) | (itu, itv) <- sus]
    adj :: A.Array Int [Int]
    adj = A.accumArray (flip (:)) [] (1, n) $ edges ++ map swap edges

    dfs :: STUArray s Int Int -> Int -> Int -> ST s Bool
    dfs col c u = do
      cu <- readArray col u
      if cu == -1
        then do
          c <- pure $ max c 0
          writeArray col u c
          and <$> forM (adj A.! u) (dfs col (1 - c))
        else return $ c == -1 || c == cu

-------------------------- Template ------------------------------------------
-- Scanner
type Scanner = State [C.ByteString]

runScanner :: Scanner a -> C.ByteString -> a
runScanner = runScannerWith C.words

runScannerWith :: (C.ByteString -> [C.ByteString]) -> Scanner a -> C.ByteString -> a
runScannerWith t s = evalState s . t

bstr :: Scanner C.ByteString
bstr = get >>= \case s : ss -> put ss >> return s

str :: Scanner String
str = C.unpack <$> bstr

int :: Scanner Int
int = fst . fromJust . C.readInt <$> bstr

numberOf :: Scanner a -> Scanner [a]
numberOf s = int >>= flip replicateM s

many :: Scanner a -> Scanner [a]
many s = get >>= \case [] -> return []; _ -> (:) <$> s <*> many s

times :: Int -> Scanner a -> Scanner [a]
times = replicateM

(><) = times

pair :: Scanner a -> Scanner b -> Scanner (a, b)
pair = liftA2 (,)

showB :: Show a => a -> C.ByteString
showB = C.pack . show
