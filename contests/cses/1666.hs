{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
-- https://cses.fi/problemset/task/1666
-- Building Roads
{-# LANGUAGE Rank2Types #-}

import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Monad.ST
import Control.Monad.State
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (catMaybes, fromJust)

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

times :: Int -> Scanner a -> Scanner [a]
times = replicateM

pair :: Scanner a -> Scanner b -> Scanner (a, b)
pair = liftA2 (,)

main :: IO ()
main = C.interact $ runScanner input >>> solve >>> C.pack
  where
    input = do
      n <- int
      m <- int
      es <- times m (pair int int)
      return $ (n, m) : es

type Edge = Int

type AdjList = Array Int [Edge]

buildGraph :: Int -> [(Int, Int)] -> AdjList
buildGraph n es = runST $ do
  adj <- newArray (1, n) [] :: ST s (STArray s Int [Edge])
  mapM_ (\(u, v) -> readArray adj u >>= writeArray adj u . (v :)) es
  mapM_ (\(u, v) -> readArray adj v >>= writeArray adj v . (u :)) es
  freeze adj

solve :: [(Int, Int)] -> String
solve ((n, m) : es) = unlines $ show (length vs) : [show v ++ " " ++ show u | u <- vs]
  where
    g = buildGraph n es
    (v : vs) = runST $ do
      vis <- newArray (1, n) False :: ST s (STUArray s Int Bool)
      xs <- mapM (dfs vis) [1 .. n]
      return $ catMaybes xs
    dfs :: STUArray s Int Bool -> Int -> ST s (Maybe Int)
    dfs vis u = do
      seen <- readArray vis u
      if seen
        then return Nothing
        else do
          writeArray vis u True
          mapM_ (dfs vis) (g ! u)
          return $ Just u
