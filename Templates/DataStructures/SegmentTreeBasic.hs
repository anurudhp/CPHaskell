module SegmentTree where

data ST a
  = STNil -- null node
  | STNode Int Int a (ST a) (ST a) -- [l, r) value (left) (right)
  deriving (Show)

value :: (Monoid a) => ST a -> a
value STNil = mempty
value (STNode _ _ v _ _) = v

buildAux :: (Monoid a) => Int -> Int -> [a] -> ST a
buildAux _ _ [] = STNil
buildAux l r [x] = STNode l r x STNil STNil
buildAux l r xs = STNode l r v lt rt
  where
    m = (l + r) `div` 2
    (ls, rs) = splitAt (m - l) xs
    lt = buildAux l m ls
    rt = buildAux m r rs
    v = value lt `mappend` value rt

-- Build a Segment Tree, with 0-indexed values
-- The value type has to be an instance of Monoid
build :: (Monoid a) => [a] -> ST a
build xs = buildAux 0 (length xs) xs

-- Query a range
-- query l r T
-- `l`, `r` - range to query [l, r)
-- `T` - segment tree
query :: (Monoid a) => Int -> Int -> ST a -> a
query _ _ STNil = mempty
query l' r' (STNode l r v lt rt)
  | l' >= r || r' <= l = mempty
  | l' <= l && r <= r' = v
  | otherwise = query l' r' lt `mappend` query l' r' rt

-- Update a value at an index
-- update i f T
-- `i` - index to update
-- `f` - function that takes the old value and returns the new value
-- `T` - segment tree
update :: (Monoid a) => Int -> (a -> a) -> ST a -> ST a
update _ _ STNil = STNil
update i f t@(STNode l r v lt rt)
  | i < l || i >= r = t
  | r - l == 1 = STNode l r (f v) lt rt
  | otherwise = STNode l r v' lt' rt'
  where
    lt' = update i f lt
    rt' = update i f rt
    v' = value lt' `mappend` value rt'
