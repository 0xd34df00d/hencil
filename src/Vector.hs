data Vector = Vector { x :: Double, y :: Double } deriving (Eq, Ord, Show)

instance Num Vector where
    p1 + p2 = Vector (x p1 + x p2) (y p1 + y p2)
    p1 - p2 = p1 + negate p2
    p1 * p2 = Vector (x p1 * x p2) (y p1 * y p2)
    abs (Vector x y) = Vector (sqrt (x * x + y * y)) 0
    negate (Vector x y) = Vector (-x) (-y)
    fromInteger x = Vector (fromInteger x) 0
    signum (Vector x y) = let m = sqrt (x * x + y * y) in Vector (x / m) (y / m)

p .* s = Vector (x p * s) (y p * s)
(*.) = flip (.*)
