{-# LANGUAGE Rank2Types #-}
module UnionFind where

import Control.Monad
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
  u <- find u dsu
  v <- find v dsu
  when (u /= v) $ do
    su <- readArray sz u -- su = sz[pu]
    sv <- readArray sz v -- sv = sz[pv]
    let s = su + sv
    (u, v) <- pure $ if su > sv then (u, v) else (v, u)
    writeArray par v u -- par[v] = u
    writeArray sz u s -- sz[u] = s
  return $ u /= v

same :: Int -> Int -> DSU s -> ST s Bool
same u v dsu = (==) <$> find u dsu <*> find v dsu

-- finalized
data DSUF = DSUF {parent :: UArray Int Int, size :: UArray Int Int}

finalize :: (forall s. ST s (DSU s)) -> DSUF
finalize st = runST $ do
  (DSU p s) <- st
  p' <- freeze p
  s' <- freeze s
  return $ DSUF p' s'

