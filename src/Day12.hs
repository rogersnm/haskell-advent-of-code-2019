module Day12 where

import Data.List (delete, elemIndex)
import Data.Maybe (fromJust)

data XYZ = XYZ Int Int Int deriving (Show, Eq)

data Position = Position Int deriving (Show, Eq)
data Velocity = Velocity Int deriving (Show, Eq)

data Planet = Planet Position Velocity deriving (Show, Eq)

getPosition (Planet position velocity) = position

newPlanet pos = Planet pos (Velocity 0)

calcChange a b
    | a == b = 0
    | a < b = 1
    | otherwise = -1

velocityChange :: Position -> Position -> Int
velocityChange (Position x1) (Position x2) = calcChange x1 x2

applyVelocity :: Position -> Velocity -> Position
applyVelocity (Position x1) (Velocity x2) = Position (x1 + x2)

step planets =
    go planets
    where go [] = []
          go (x:xs) =
            let otherPlanets = delete x planets
                (Planet xPos xVel) = x
                (Velocity v) = xVel
                velocityChanges = map ((velocityChange xPos) . getPosition) otherPlanets
                vChange = sum velocityChanges
                xVel' = Velocity (v + vChange)
                xPos' = applyVelocity xPos xVel'
            in (Planet xPos' xVel') : (go xs)

potentialEnergy (Planet (Position x) _) = abs x
kineticEnergy (Planet _ (Velocity x)) = abs x

simulate planets 0 = planets
simulate planets steps = simulate (step planets) (steps - 1)

stepsToRepeat planetsIn =
    go planetsIn 1
    where go planets stepCount = let planets' = step planets in if planets' == planetsIn then stepCount else go planets' (stepCount + 1)

stepsToRepeatDim planetsIn =
    go planetsIn 1 [planetsIn]
    where go planets stepCount observedValues =
            let planets' = step planets
                alreadyObserved = planets' `elem` observedValues
                observedAt = (length observedValues - 1) - fromJust (planets' `elemIndex` observedValues)
                cycle = stepCount - observedAt
            in if alreadyObserved then cycle else go planets' (stepCount + 1) (planets':observedValues)

-- planAPos = XYZ (-1) 0 2
-- planBPos = XYZ 2 (-10) (-7)
-- planCPos = XYZ 4 (-8) 8
-- planDPos = XYZ 3 5 (-1)

-- planAPos = XYZ (-8) (-10) 0
-- planBPos = XYZ 5 5 10
-- planCPos = XYZ 2 (-7) 3
-- planDPos = XYZ 9 (-8) (-3)

planAPos = XYZ 17 (-9) 4
planBPos = XYZ 2 2 (-13)
planCPos = XYZ (-1) 5 (-1)
planDPos = XYZ 4 7 (-7)

getX (XYZ x _ _) = x
getY (XYZ _ y _) = y
getZ (XYZ _ _ z) = z

fullPos =  [planAPos, planBPos, planCPos, planDPos]

xDim = map (newPlanet . Position . getX) fullPos
yDim = map (newPlanet . Position . getY) fullPos
zDim = map (newPlanet . Position . getZ) fullPos

-- Part 1

numSteps = 1000

calcEnergy dimension =
    let vals = simulate dimension numSteps
        pot = map potentialEnergy vals
        kin = map kineticEnergy vals
    in (pot, kin)

(potX, kinX) = calcEnergy xDim
(potY, kinY) = calcEnergy yDim
(potZ, kinZ) = calcEnergy zDim

combine [] [] [] = []
combine (x:xs) (y:ys) (z:zs) = (x + y + z) : combine xs ys zs

pot = combine potX potY potZ
kin = combine kinX kinY kinZ

planetEnergy [] [] = []
planetEnergy (x:xs) (y:ys) = (x * y) : planetEnergy xs ys
energy = planetEnergy pot kin

part1 = sum energy

-- Part 2

xSteps = stepsToRepeat xDim
ySteps = stepsToRepeat yDim
zSteps = stepsToRepeat zDim

lcmm :: Integral a => [a] -> a
lcmm [] = 1
lcmm (x:xs) = lcm x (lcmm xs)

part2 = lcmm [xSteps, ySteps, zSteps]
