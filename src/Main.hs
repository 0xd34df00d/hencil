import Data.List
import Debug.Trace

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

unitV φ = Vector (- sin φ) (cos φ)

data PenState = PenState {
            penCenter :: Vector,
            penVelocity :: Vector,
            φ :: Double
        } deriving (Show)

data Forces = Forces {
            mgΣ :: Double,
            mgVec :: Vector,
            fRVec :: Vector
        } deriving (Show)

μ = 1.0
δt = 0.001

initState φ = PenState (unitV φ) (Vector 0 0) φ

stepState :: PenState -> PenState
stepState ps = (fΣ, fs) `traceShow` ps'
    where fs = calcForces ps
          fΣ = Vector ((x $ fRVec fs)) (mgΣ fs - y (fRVec fs))
          δv = negate $ fΣ .* δt
          δc = penVelocity ps .* δt + δv .* (δt / 2)
          c' = penCenter ps + δc
          ps' = ps { penCenter = c', penVelocity = penVelocity ps + δv, φ = acos (y c') }

calcForces :: PenState -> Forces
calcForces ps = let f = Forces mg (Vector mgT mgN) (Vector (min mgT μ * mgN) mgN) in f
    where mg = 1
          mgN = mg * cos (φ ps)
          mgT = mg * sin (φ ps)

whole = unfoldr f
    where f st | y (penCenter st') >= 0 = Just (st', st')
               | otherwise = Nothing
            where st' = stepState st

toFile :: [PenState] -> String
toFile = intercalate "\n" . map ss
    where ss (PenState (Vector x y) _ _) = show x ++ " " ++ show y
