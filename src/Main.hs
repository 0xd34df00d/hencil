{-# LANGUAGE ViewPatterns #-}

import Data.List
import Control.Arrow
import Debug.Trace

δt = 0.001
g = 10
l = 1
m = 1

data PenState = PenState {
            μ :: Double,
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

initState μ φ = PenState μ 0 φ 0 0

stepState :: PenState -> (PenState, Forces)
stepState ps@(PenState μ xPos φ v ω) = let ps = PenState μ (xPos + δx) (φ + δφ) (v + δv) (ω + δω) in {-(ps, fs) `traceShow`-} (ps, fs)
    where fs@(Forces φ'' fN fH fF) = calcForces ps
          δω = φ'' * δt
          δφ = ω * δt + δω * δt / 2
          δv = δt * (fH - fF) / m
          δx = v * δt + δv * δt / 2

calcForces :: PenState -> Forces
calcForces (PenState μ _ φ v ω) = Forces φ'' fN fH fF
    where φ'' = 3 * g * sin φ / (2 * l)
          aτ = φ'' * l
          an = ω ^ 2 * l
          av = aτ * sin φ + an * cos φ
          ah = aτ * cos φ - an * sin φ
          fN = max 0 $ m * g - m * av
          fH = m * ah
          fF | abs fH <= μ * fN = fH
             | otherwise = signum fH * μ * fN 

whole = unfoldr f
    where f st | φ st' < pi / 2 = Just ((st', fs, (xPos st' - xPos st) / δt), st')
               | otherwise = Nothing
            where (st', fs) = stepState st

toFile :: [(PenState, Forces, Double)] -> String
toFile = intercalate "\n" . map (\(PenState _ x φ _ _, _, _) -> show (x - l * sin φ) ++ " " ++ show (l * cos φ))

toFileEnd :: [(PenState, Forces, Double)] -> String
toFileEnd = intercalate "\n" . map (\(t, (PenState _ x φ _ _, Forces _ fN _ fF, v)) -> show (t * δt) ++ " " ++ show x ++ " " ++ show (-fN / g) ++ " " ++ show (-fF / g) ++ " " ++ show v) . zip [1.0,2.0..]

makeEndPoss :: [Double] -> [(Double, Double)]
makeEndPoss = map $ id &&& (\(last . whole . (`initState` 0.00001) -> (PenState _ xPos _ _ _, _, _)) -> xPos)

