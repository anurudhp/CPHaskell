import Data.List (sortBy)

-----------------------------
-- 2D-Vectors
-----------------------------

data Vec2D t = Vec2D t t deriving (Show)

instance Functor Vec2D where
  fmap f (Vec2D x y) = Vec2D (f x) (f y) 

nullVector :: (Num t) => Vec2D t
nullVector = let ze = fromInteger 0 in Vec2D ze ze

-- vector addition
infixl 6 |+|
(|+|) :: (Num t) => Vec2D t -> Vec2D t -> Vec2D t
(Vec2D x y) |+| (Vec2D x' y') = Vec2D (x + x') (y + y')

-- vector subtraction
infixl 6 |-|
(|-|) :: (Num t) => Vec2D t -> Vec2D t -> Vec2D t
(Vec2D x y) |-| (Vec2D x' y') = Vec2D (x - x') (y - y')

-- dot product
infixl 8 |.|
(|.|) :: (Num t) => Vec2D t -> Vec2D t -> t
(Vec2D x y) |.| (Vec2D x' y') =  (x * x') + (y * y')

-- cross product
infixl 9 |><| 
(|><|) :: (Num t) => Vec2D t -> Vec2D t -> t
(Vec2D x y) |><| (Vec2D x' y') = x * y' - y * x'

-- scaling
infixl 7 |*|
(|*|) :: (Num t) => t -> Vec2D t -> Vec2D t
(|*|) s v = fmap (*s) v

infixl 7 |/|
(|/|) :: (Floating t) => Vec2D t -> t -> Vec2D t
(|/|) v s = fmap (/s) v

-- other operations
norm :: (Floating t) => Vec2D t -> t
norm (Vec2D x y) = sqrt $ x * x + y * y

unit :: (Floating t) => Vec2D t -> Vec2D t
unit v = v |/| (norm v)

infixl 6 `projectOnto`
projectOnto :: (Floating t) => Vec2D t -> Vec2D t -> Vec2D t
projectOnto u v = let vc = unit v in (u |.| vc) |*| vc

rotateCCW :: (Floating t) => t -> Vec2D t -> Vec2D t
rotateCCW angle (Vec2D x y) = 
  let (cs, sn) = (cos angle, sin angle) in
  Vec2D 
    (cs * x - sn * y)
    (sn * x + cs * y)

type RotDir t = Vec2D t -> Vec2D t -> Vec2D t -> Bool

isCCW :: (Ord t, Num t) => RotDir t
isCCW u v w = ((w |-| v) |><| (v |-| u)) >= 0

isCollinear :: (Eq t, Num t) => RotDir t
isCollinear u v w = ((w |-| v) |><| (v |-| u)) == 0

isCCWStrict :: (Ord t, Num t) => RotDir t
isCCWStrict u v w = (isCCW u v w) && (not $ isCollinear u v w)

isCW :: (Ord t, Num t) => RotDir t
isCW u v w = not $ isCCWStrict u v w

isCWStrict :: (Ord t, Num t) => RotDir t
isCWStrict u v w = not $ isCCW u v w

-----------------------------
-- Lines
-----------------------------

-- Line2D a, v => l(r) = a |+| r|*|v
data Line2D t = Line2D (Vec2D t) (Vec2D t)

lineFromTwoPoints :: (Num t) => Vec2D t -> Vec2D t -> Line2D t
lineFromTwoPoints a b = Line2D a (b |-| a)

dropPerpendicular :: (Floating t) => Vec2D t -> Line2D t -> Line2D t
dropPerpendicular p (Line2D _ v) = Line2D p (p |+| (rotateCCW (pi/2) v))

-----------------------------
-- Polygons
-----------------------------

type Polygon t = [Vec2D t]

convexArea :: (Floating t) => Polygon t -> t
convexArea [] = 0
convexArea poly = (/2) $ sum $ map (\(p,q) -> p |><| q) 
                    $ zip poly (tail poly ++ [head poly])

convexHull :: (Num t, Ord t) => Polygon t -> Polygon t
convexHull poly
  | length poly <= 3 = poly
  | otherwise = init dn ++ tail up
  where
    points = reverse $ sortBy (\(Vec2D x _) (Vec2D x' _) -> compare x x') poly
    up = compute isCWStrict points
    dn = compute isCCWStrict points
    compute :: (Num t, Ord t) => RotDir t -> Polygon t -> Polygon t
    compute _ [] = []
    compute rot (p:rest) = p : remove (compute rot rest)
      where
        remove [] = []
        remove [q] = [q]
        remove xs@(q:r:rest)
          | not $ rot r q p = remove (r:rest)
          | otherwise = xs
