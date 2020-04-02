-----------------------------
-- 3D-Vectors
-----------------------------

data Vec3D t = Vec3D t t t deriving (Show, Eq)

instance Functor Vec3D where
  fmap f (Vec3D x y z) = Vec3D (f x) (f y) (f z) 

nullVector :: (Num t) => Vec3D t
nullVector = let ze = fromInteger 0 in Vec3D ze ze ze

-- vector addition
infixl 6 |+|
(|+|) :: (Num t) => Vec3D t -> Vec3D t -> Vec3D t
(Vec3D x y z) |+| (Vec3D x' y' z') = Vec3D (x + x') (y + y') (z + z')

-- vector subtraction
infixl 6 |-|
(|-|) :: (Num t) => Vec3D t -> Vec3D t -> Vec3D t
(Vec3D x y z) |-| (Vec3D x' y' z') = Vec3D (x - x') (y - y') (z - z')

-- dot product
infixl 8 |.|
(|.|) :: (Num t) => Vec3D t -> Vec3D t -> t
(Vec3D x y z) |.| (Vec3D x' y' z') =  (x * x') + (y * y') + (z * z')

-- cross product
infixl 9 |><| 
(|><|) :: (Num t) => Vec3D t -> Vec3D t -> Vec3D t
(Vec3D x y z) |><| (Vec3D x' y' z') = Vec3D 
                                        (y * z' - z * y')
                                        (z * x' - x * z')
                                        (x * y' - y * x')

-- scaling
infixl 7 |*|
(|*|) :: (Num t) => t -> Vec3D t -> Vec3D t
(|*|) s v = fmap (*s) v

infixl 7 |/|
(|/|) :: (Floating t) => Vec3D t -> t -> Vec3D t
(|/|) v s = fmap (/s) v

-- other operations
norm2 :: (Num t) => Vec3D t -> t
norm2 (Vec3D x y z) = x * x + y * y + z * z

norm :: (Floating t) => Vec3D t -> t
norm = sqrt . norm2

unit :: (Floating t) => Vec3D t -> Vec3D t
unit v = v |/| (norm v)

-- project u onto v
infixl 6 `projectOnto`
projectOnto :: (Floating t) => Vec3D t -> Vec3D t -> Vec3D t
projectOnto u v = let vc = unit v in (u |.| vc) |*| vc

rotateCCW :: (Floating t) => Vec3D t -> t -> Vec3D t -> Vec3D t
rotateCCW axis angle v = 
  (cos angle) |*| v 
  |+| (sin angle) |*| v |><| axis
  |+| (1.0 - cos angle) * (v |.| axis) |*| axis

boxProduct :: (Num t) => Vec3D t -> Vec3D t -> Vec3D t -> t
boxProduct u v w = u |.| v |><| w

------------------------------
-- Lines
------------------------------

-- Line3D a v => through a, along v
data Line3D t = Line3D (Vec3D t) (Vec3D t) deriving (Show)

instance (Num t, Eq t) => Eq (Line3D t) where
  (Line3D a v) == (Line3D a' v') =
    v |><| v' == nullVector
    && v |><| (a |-| a') == nullVector

lineFromTwoPoints :: (Num t) => Vec3D t -> Vec3D t -> Line3D t
lineFromTwoPoints a b = Line3D a (b |-| a)

areParallel :: (Num t, Eq t) => Line3D t -> Line3D t -> Bool
areParallel (Line3D a v) (Line3D a' v') = v |><| v' == nullVector

areParallelButNotSame :: (Num t, Eq t) => Line3D t -> Line3D t -> Bool
areParallelButNotSame l l' = areParallel l l' && not (l == l')

areIntersecting :: (Num t, Eq t) => Line3D t -> Line3D t -> Bool
areIntersecting l@(Line3D a v) l'@(Line3D a' v') =
  not $ areParallel l l' || boxProduct v v' (a |-| a') == 0

intersection :: (Floating t, Eq t) => Line3D t -> Line3D t -> Maybe (Vec3D t)
intersection l@(Line3D a v) l'@(Line3D a' v')
  | areIntersecting l l' = Just $ a |+| k |*| v
  | otherwise = Nothing
    where
      p = v |><| v'
      k = (boxProduct p (a |-| a') v') / (p |.| p)

areSkew :: (Num t, Eq t) => Line3D t -> Line3D t -> Bool
areSkew l l' = not $ areParallel l l' || areIntersecting l l'

---------------------------
-- Planes
---------------------------
-- Plane3D a n => contains a, normal n
data Plane3D t = Plane3D (Vec3D t) (Vec3D t) 

planeFromTriangle :: (Num t) => Vec3D t -> Vec3D t -> Vec3D t -> Plane3D t
planeFromTriangle p q r = Plane3D p (p |><| q |+| q |><| r |+| r |><| p)

planeFromLines :: (Num t) => Line3D t -> Line3D t -> Plane3D t
planeFromLines (Line3D a v) (Line3D a' v') = Plane3D a (v |><| (a |-| a')) 

distFromPlane :: (Floating t) => Plane3D t -> Vec3D t -> t
distFromPlane (Plane3D a n) p = 
  (norm $ p `projectOnto` n) - (norm $ a `projectOnto` n)

projectPointOntoPlane :: (Floating t) => Plane3D t -> Vec3D t -> Vec3D t
projectPointOntoPlane (Plane3D a n) p = 
  p |-| (p `projectOnto` n) |+| (norm $ a `projectOnto` n) |*| n
