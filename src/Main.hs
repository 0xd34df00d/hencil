import Data.List
import Debug.Trace

μ = 1.0
δt = 0.01
g = 1
l = 1
m = 1

data PenState = PenState {
            xPos :: Double,
            φ :: Double,
            v :: Double,
            ω :: Double
        } deriving (Show)

data Forces = Forces {
            φ'' :: Double,
            fN :: Double,
            fF :: Double
        } deriving (Show)

initState φ = PenState 0 φ 0 0

stepState :: PenState -> PenState
stepState ps@(PenState xPos φ v ω) = let ps = PenState (xPos + δx) (φ + δφ) (v + δv) (ω + δω) in ps `traceShow` ps
    where fs@(Forces φ'' fN fF) = calcForces ps
          δω = φ'' * δt
          δφ = ω * δt + δω * δt / 2
          δv = fF / m
          δx = {-v * δt +-} δv * δt / 2

calcForces :: PenState -> Forces
calcForces (PenState _ φ _ _) = Forces φ'' fN fF
    where φ'' = 3 * g * sin φ / l ^ 2
          ac = φ'' * l / 2
          fN = m * g - m * ac * sin φ
          fF = max 0 $ m * ac * cos φ - μ * (m * g - m * ac * sin φ)

whole = unfoldr f
    where f st | φ st' < pi / 2 = Just (st', st')
               | otherwise = Nothing
            where st' = stepState st

toFile :: [PenState] -> String
toFile = intercalate "\n" . map (\(PenState x φ _ _) -> show (x - l * sin φ) ++ " " ++ show (l * cos φ))
