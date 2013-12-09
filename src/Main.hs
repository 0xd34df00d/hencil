import Data.List
import Debug.Trace

μ = 1.0
δt = 0.001
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
            fH :: Double,
            fF :: Double
        } deriving (Show)

initState φ = PenState 0 φ 0 0

stepState :: PenState -> PenState
stepState ps@(PenState xPos φ v ω) = let ps = PenState (xPos + δx) (φ + δφ) (v + δv) (ω + δω) in (ps, fs) `traceShow` ps
    where fs@(Forces φ'' fN fH fF) = calcForces ps
          δω = φ'' * δt
          δφ = ω * δt + δω * δt / 2
          δv = δt * (fH - fF) / m
          δx = v * δt + δv * δt / 2

calcForces :: PenState -> Forces
calcForces (PenState _ φ v ω) = Forces φ'' fN fH fF
    where φ'' = 3 * g * sin φ / (2 * l)
          aτ = φ'' * l
          an = ω ^ 2 * l
          av = aτ * sin φ + an * cos φ
          ah = aτ * cos φ - an * sin φ
          fN = m * g - m * av
          fH = m * ah
          fF | abs fH <= μ * fN = fH
             | otherwise = signum fH * μ * fN 

whole = unfoldr f
    where f st | φ st' < pi / 2 = Just (st', st')
               | otherwise = Nothing
            where st' = stepState st

toFile :: [PenState] -> String
toFile = intercalate "\n" . map (\(PenState x φ _ _) -> show (x - l * sin φ) ++ " " ++ show (l * cos φ))
