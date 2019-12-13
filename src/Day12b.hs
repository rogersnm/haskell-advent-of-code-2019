module Day12b where

import Data.List (delete, elemIndex)
import Data.Maybe (fromJust)

data XYZ = XYZ Int Int Int deriving (Show, Eq)

data Position = Position XYZ deriving (Show, Eq)
data Velocity = Velocity XYZ deriving (Show, Eq)

data Planet = Planet Position Velocity deriving (Show, Eq)

getPosition (Planet position velocity) = position
getVelocity (Planet position velocity) = velocity

posToXYZ (Position xyz) = xyz
velToXYZ (Velocity xyz) = xyz

newPlanet pos = Planet pos (Velocity (XYZ 0 0 0))

calcChange a b
    | a == b = 0
    | a < b = 1
    | otherwise = -1

velocityChange :: Position -> Position -> XYZ
velocityChange (Position (XYZ x1 y1 z1)) (Position (XYZ x2 y2 z2)) =
    let vx = calcChange x1 x2
        vy = calcChange y1 y2
        vz = calcChange z1 z2
    in XYZ vx vy vz

addXYZ (XYZ x1 y1 z1) (XYZ x2 y2 z2) = XYZ (x1 + x2) (y1 + y2) (z1 + z2)

applyVelocity :: Position -> Velocity -> Position
applyVelocity (Position (XYZ x1 y1 z1)) (Velocity (XYZ x2 y2 z2)) = Position (XYZ (x1 + x2) (y1 + y2) (z1 + z2))

step planets =
    go planets
    where go [] = []
          go (x:xs) =
            let otherPlanets = delete x planets
                xPos :: Position
                xPos = getPosition x
                xVel :: Velocity
                xVel = getVelocity x
                velocityChanges :: [XYZ]
                velocityChanges = map ((velocityChange xPos) . getPosition) otherPlanets
                vChange :: XYZ
                vChange = foldr addXYZ (XYZ 0 0 0) velocityChanges
                xVel' = Velocity (addXYZ (velToXYZ xVel) vChange)
                xPos' = applyVelocity xPos xVel'
            in (Planet xPos' xVel') : (go xs)


potentialEnergy (Planet (Position (XYZ x y z)) _) = abs x + abs y + abs z
kineticEnergy (Planet _ (Velocity (XYZ x y z))) = abs x + abs y + abs z
combinedEnergy pot kin = pot * kin
energy planet = combinedEnergy (potentialEnergy planet) (kineticEnergy planet)

totalEnergy planets = let energies = map energy planets in sum energies

simulate planets 0 = planets
simulate planets steps = simulate (step planets) (steps - 1)

stepsToRepeat planetsIn =
    go planetsIn 1
    where go planets stepCount = let planets' = step planets in if planets' == planetsIn then stepCount else go planets' (stepCount + 1)


stepsToRepeatDim planetsIn collector dimFunc =
    go planetsIn 1 [(collector dimFunc planetsIn)]
    where go planets stepCount observedValues =
            let planets' = step planets
                dim = collector dimFunc planets'
                alreadyObserved = dim `elem` observedValues
                observedAt = (length observedValues - 1) - fromJust (dim `elemIndex` observedValues)
                cycle = stepCount - observedAt
            in if alreadyObserved then cycle else go planets' (stepCount + 1) (dim:observedValues)

-- collector dimFunc planets = map dimFunc planets

dimPx (Planet (Position (XYZ x y z)) (Velocity (XYZ vx vy vz))) = (x, vx)
dimPy (Planet (Position (XYZ x y z)) (Velocity (XYZ vx vy vz))) = (y, vy)
dimPz (Planet (Position (XYZ x y z)) (Velocity (XYZ vx vy vz))) = (z, vz)
-- dimVx (Planet (Position (XYZ x y z)) (Velocity (XYZ vx vy vz))) = vx
-- dimVy (Planet (Position (XYZ x y z)) (Velocity (XYZ vx vy vz))) = vy
-- dimVz (Planet (Position (XYZ x y z)) (Velocity (XYZ vx vy vz))) = vz

dims = [dimPx, dimPy, dimPz] --, dimVx, dimVy, dimVz]

periods planets = map (stepsToRepeatDim planets map) dims

-- planAPos = Position (XYZ (-1) 0 2)
-- planBPos = Position (XYZ 2 (-10) (-7))
-- planCPos = Position (XYZ 4 (-8) 8)
-- planDPos = Position (XYZ 3 5 (-1))


-- planAPos = Position (XYZ (-8) (-10) 0)
-- planBPos = Position (XYZ 5 5 10)
-- planCPos = Position (XYZ 2 (-7) 3)
-- planDPos = Position (XYZ 9 (-8) (-3))

planAPos = Position (XYZ 17 (-9) 4)
planBPos = Position (XYZ 2 2 (-13))
planCPos = Position (XYZ (-1) 5 (-1))
planDPos = Position (XYZ 4 7 (-7))

initialVelocity = Velocity (XYZ 0 0 0)
lcmm :: Integral a => [a] -> a
lcmm [] = 1
lcmm (x:xs) = lcm x (lcmm xs)

planA = newPlanet planAPos
planB = newPlanet planBPos
planC = newPlanet planCPos
planD = newPlanet planDPos

plans = [planA, planB, planC, planD]