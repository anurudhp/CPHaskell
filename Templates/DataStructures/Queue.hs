module Queue where

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
