{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}

import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Monad
import Control.Monad.ST
import Control.Monad.State (State, evalState, get, gets, put)
import Data.Array.ST
import Data.Array.Unboxed
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (fromJust)
import Data.STRef.Strict

main :: IO ()
main = C.interact $ runScanner input >>> solve >>> showB

type PII = (Int, Int)

type Input = (Int, Int, [PII])

input :: Scanner Input
input = do
  !m <- int
  !n <- int
  (m,,) <$> int <*> n >< pair int int

solve :: Input -> Int
solve (!m, !s, !ts) = runST $ do
  !q <- newArray (0, m - 1) 0 :: ST s (STUArray s Int Int)
  !dis <- newArray (0, m - 1) (-1) :: ST s (STUArray s Int Int)
  !tref <- newSTRef 0

  writeArray q 0 s
  writeArray dis s 0

  forM_ [0 .. m -1] $ \h -> do
    !t <- readSTRef tref
    when (h <= t) $ do
      !u <- readArray q h
      !d <- readArray dis u
      forM_ ts $ \(a, b) -> do
        let v = (u * a + b) `mod` m
        !d' <- readArray dis v
        when (d' == -1) $ do
          writeArray dis v (d + 1)
          modifySTRef' tref (+ 1)
          !t <- readSTRef tref
          writeArray q t v
  readArray dis 0

-------------------------- Template ------------------------------------------
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

numberOf :: Scanner a -> Scanner [a]
numberOf s = int >>= flip replicateM s

times :: Int -> Scanner a -> Scanner [a]
times = replicateM

(><) = times

pair :: Scanner a -> Scanner b -> Scanner (a, b)
pair = liftA2 (,)

showB :: Show a => a -> C.ByteString
showB = C.pack . show
