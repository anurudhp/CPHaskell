module Heap where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.STRef

data Heap a s = Heap {size :: STRef s Int, heapST :: STArray s Int a}

mkHeap :: (Ord a) => Int -> ST s (Heap a s)
mkHeap n = Heap <$> newSTRef 0 <*> newArray_ (0, n)

swap :: Int -> Int -> Heap a s -> ST s ()
swap i j (Heap _ arr) = do
  u <- readArray arr i
  v <- readArray arr j
  writeArray arr i v
  writeArray arr j u

reheap :: (Ord a) => Int -> Int -> Heap a s -> ST s ()
reheap idx lim heap@(Heap szptr arr) = when (idx < lim) $ do
  u <- readArray arr idx
  when (idx > 0) $ do
    let pidx = idx `div` 2
    v <- readArray arr pidx
    when (u < v) $ do
      swap idx pidx heap
      reheap pidx lim heap
  cidx <- snd . minimum . catMaybes
            <$> forM [idx, 2 * idx, 2 * idx + 1]
              (\cidx -> do
                 if cidx >= lim
                   then return Nothing
                   else do
                     v <- readArray arr cidx
                     return $ Just (v, cidx))
  when (cidx /= idx) $ do
    swap idx cidx heap
    reheap cidx lim heap

insert :: (Ord a) => a -> Heap a s -> ST s ()
insert a heap@(Heap szptr arr) = do
  sz <- readSTRef szptr
  writeArray arr sz a
  writeSTRef szptr (sz + 1)
  reheap sz (sz + 1) heap

top :: Heap a s -> ST s a
top (Heap _ arr) = readArray arr 0

empty :: Heap a s -> ST s Bool
empty (Heap sz _) = (== 0) <$> readSTRef sz

pop :: (Ord a) => Heap a s -> ST s a
pop heap@(Heap szptr arr) = do
  t <- top heap
  sz <- subtract 1 <$> readSTRef szptr
  readArray arr sz >>= writeArray arr 0
  writeSTRef szptr sz
  reheap 0 sz heap
  return t

heapSortST :: (Ord a) => [a] -> ST s [a]
heapSortST xs = do
  let n = length xs
  h <- mkHeap n
  forM_ xs (`insert` h)
  replicateM n (pop h)

heapSort :: (Ord a) => [a] -> [a]
heapSort xs = runST (heapSortST xs)
