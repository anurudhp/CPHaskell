{-# LANGUAGE Rank2Types #-}
module UnionFind where

import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

data DSU s = DSU {parentST :: STUArray s Int Int, sizeST :: STUArray s Int Int}

mkDSU :: Int -> ST s (DSU s)
mkDSU n = DSU <$> newListArray (1, n) [1..n] <*> newArray (1, n) 1

find :: Int -> DSU s -> ST s Int
find u dsu@(DSU par _) = do
  p <- readArray par u
  if p == u
     then return u
     else do
       p' <- find p dsu
       writeArray par u p'
       return p'

merge :: Int -> Int -> DSU s -> ST s Bool
merge u v dsu@(DSU par sz) = do
  pu <- find u dsu
  pv <- find v dsu
  if pu == pv
     then return False
     else do
       su <- readArray sz pu -- su = sz[pu]
       sv <- readArray sz pv -- sv = sz[pv]
       let s = su + sv
       let (p, w) = if su > sv then (pu, pv) else (pv, pu)
       writeArray par w p -- par[w] = p
       writeArray sz p s -- sz[p] = s
       return True

-- finalized
data DSUF = DSUF {parent :: UArray Int Int, size :: UArray Int Int}

finalize :: (forall s. ST s (DSU s)) -> DSUF
finalize st = runST $ do
  (DSU p s) <- st
  p' <- freeze p
  s' <- freeze s
  return $ DSUF p' s'

