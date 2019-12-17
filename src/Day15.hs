module Day15 where

import Data.List (find, nub, delete, sort, sortOn)
import System.IO
import Data.Maybe (fromJust, Maybe, isNothing)
import Data.Array.Unboxed

instructionsList :: [Int]
instructionsList = [3,1033,1008,1033,1,1032,1005,1032,31,1008,1033,2,1032,1005,1032,58,1008,1033,3,1032,1005,1032,81,1008,1033,4,1032,1005,1032,104,99,1002,1034,1,1039,101,0,1036,1041,1001,1035,-1,1040,1008,1038,0,1043,102,-1,1043,1032,1,1037,1032,1042,1105,1,124,1002,1034,1,1039,101,0,1036,1041,1001,1035,1,1040,1008,1038,0,1043,1,1037,1038,1042,1105,1,124,1001,1034,-1,1039,1008,1036,0,1041,101,0,1035,1040,101,0,1038,1043,1002,1037,1,1042,1105,1,124,1001,1034,1,1039,1008,1036,0,1041,1002,1035,1,1040,102,1,1038,1043,1002,1037,1,1042,1006,1039,217,1006,1040,217,1008,1039,40,1032,1005,1032,217,1008,1040,40,1032,1005,1032,217,1008,1039,39,1032,1006,1032,165,1008,1040,39,1032,1006,1032,165,1101,0,2,1044,1105,1,224,2,1041,1043,1032,1006,1032,179,1102,1,1,1044,1106,0,224,1,1041,1043,1032,1006,1032,217,1,1042,1043,1032,1001,1032,-1,1032,1002,1032,39,1032,1,1032,1039,1032,101,-1,1032,1032,101,252,1032,211,1007,0,74,1044,1106,0,224,1101,0,0,1044,1106,0,224,1006,1044,247,102,1,1039,1034,102,1,1040,1035,1002,1041,1,1036,1002,1043,1,1038,1002,1042,1,1037,4,1044,1105,1,0,15,82,44,17,88,23,99,42,83,68,98,44,75,66,15,14,89,20,34,89,18,1,84,70,84,69,55,89,65,10,76,63,83,20,80,60,48,47,98,65,82,84,68,89,52,76,63,86,61,75,4,52,82,79,24,28,93,94,95,40,66,76,81,50,31,94,81,54,19,91,92,61,18,28,79,77,43,69,19,5,87,35,14,23,94,10,76,32,73,90,20,86,67,90,80,8,86,25,89,89,26,48,37,81,49,25,87,92,17,46,84,96,95,60,79,52,19,13,93,30,93,99,17,13,89,96,36,93,81,89,18,2,97,42,45,63,86,20,26,76,97,29,75,56,7,97,93,2,78,9,79,8,57,84,38,80,53,98,89,34,71,85,17,96,50,31,93,64,7,81,72,85,32,83,31,99,69,90,88,33,88,81,41,80,46,47,93,75,34,95,8,98,24,7,76,77,17,23,95,72,82,98,24,91,95,50,38,92,91,32,95,40,77,80,84,82,7,90,23,13,92,40,82,37,80,56,24,79,99,64,90,55,58,46,33,4,88,92,7,84,19,45,16,75,94,40,93,21,87,94,79,39,83,52,92,14,21,77,82,5,84,85,48,75,19,26,91,28,99,87,81,86,24,53,98,52,25,2,75,39,82,24,51,77,47,92,53,94,27,34,85,22,25,36,92,79,29,2,10,19,95,13,96,82,56,99,3,91,62,99,43,49,7,91,96,77,89,7,99,86,24,92,57,24,49,3,96,77,35,75,11,86,21,1,82,67,84,90,75,96,9,83,1,47,78,7,98,30,11,88,52,78,58,98,47,90,46,78,14,77,88,3,97,87,70,75,24,98,5,80,87,93,95,22,37,59,85,23,41,89,91,9,7,90,61,3,95,96,92,25,57,47,38,88,14,15,84,31,79,20,79,77,22,33,90,70,89,78,51,24,93,81,21,79,82,17,75,88,78,26,87,24,38,96,50,81,6,46,93,39,91,92,81,39,91,5,79,58,9,87,50,83,63,87,2,29,92,37,81,55,59,99,91,35,9,96,18,82,66,4,89,44,87,92,6,79,88,9,9,63,88,71,77,91,35,29,87,87,51,20,94,19,57,93,72,89,4,77,10,87,20,67,80,79,71,1,75,28,87,88,87,55,37,80,85,5,55,5,97,12,62,88,82,27,6,99,93,42,91,16,75,80,6,20,96,6,84,6,46,84,23,92,93,32,90,79,3,54,7,97,92,92,33,79,9,5,10,90,76,19,76,1,85,83,58,2,91,83,77,59,63,89,26,97,67,96,52,88,62,65,23,91,94,51,31,80,24,5,72,40,81,9,85,79,12,98,44,45,81,25,30,60,5,76,92,62,18,32,78,25,16,76,97,18,96,39,96,60,78,78,47,99,48,82,98,57,96,98,73,89,18,12,91,8,66,85,57,94,22,76,88,98,39,58,96,91,61,98,89,7,77,91,13,96,20,86,2,88,91,27,75,32,29,79,51,81,4,86,10,37,79,84,67,49,75,20,94,91,23,33,92,38,91,37,76,79,55,91,43,80,25,98,77,91,88,44,15,97,45,3,86,73,87,30,91,62,80,80,16,85,54,88,54,75,88,65,18,85,22,90,79,36,10,77,86,65,30,38,85,3,90,44,48,75,81,80,32,59,90,91,41,95,72,79,11,66,26,96,20,4,68,88,23,95,31,98,12,98,56,94,95,80,68,78,39,79,93,85,55,96,4,77,14,80,46,95,84,84,6,93,35,95,46,85,92,81,69,85,92,87,0,0,21,21,1,10,1,0,0,0,0,0,0]

instructionsList' = instructionsList <> replicate 500 (0 :: Int)

instructions :: UArray Int Int
instructions = listArray (0, length instructionsList') instructionsList'

intToList :: Integral x => x -> [x]
intToList 0 = []
intToList x = intToList (x `div` 10) ++ [x `mod` 10]

last2 [y] = [y]
last2 [x,y] = [x,y]
last2 (x:xs) = last2 xs

listToInt :: Integral x => [x] -> x
listToInt [] = 0
listToInt (x:xs) = (x * 10 ^ length xs) + listToInt xs

getOp :: Int -> Int
getOp = listToInt . last2 . intToList

zerofill xs
    | length xs < 5 = zerofill (0:xs)
    | otherwise = xs

getFullCode = zerofill . intToList
getParamModes = take 3
getParamModesFromOpCode = getParamModes . getFullCode

replaceAt :: Int -> UArray Int Int -> Int -> UArray Int Int
replaceAt val arr pos = arr // [(pos, val)]

data Machine = Machine (UArray Int Int) Int Int deriving (Show)

getVal :: Machine -> Int -> Int -> Int
getVal (Machine program pos relativeBase) opCode argNum  =
    let paramModes = getParamModesFromOpCode opCode;
        paramMode = paramModes !! (2 - (argNum - 1))
        arg = program ! (pos + argNum)
    in
        case paramMode of
            0 -> program ! arg
            1 -> arg
            2 -> program ! (relativeBase + arg)

getStoreLoc :: Machine -> Int -> Int -> Int
getStoreLoc (Machine program pos relativeBase) opCode argNum =
    let paramModes = getParamModesFromOpCode opCode;
        paramMode = paramModes !! (2 - (argNum - 1))
        arg = program ! (pos + argNum)
    in
        case paramMode of
            0 -> arg
            2 -> relativeBase + arg

processInst :: Machine -> Machine
processInst machine =
    let (Machine program pos relativeBase) = machine
        opCode = program ! pos;
        getVal' = getVal machine opCode
        getStoreLoc' = getStoreLoc machine opCode
        getXY = (getVal' 1, getVal' 2)
        getXYZ = (getVal' 1, getVal' 2, getStoreLoc' 3)
    in
        case getOp opCode of
            1 -> let (x, y, z) = getXYZ; newProg = replaceAt (x + y) program z                   in Machine newProg (pos + 4) relativeBase
            2 -> let (x, y, z) = getXYZ; newProg = replaceAt (x * y) program z                   in Machine newProg (pos + 4) relativeBase
            5 -> let (x, y) = getXY; pos' = (if x /= 0 then y else pos + 3)                      in Machine program pos' relativeBase
            6 -> let (x, y) = getXY; pos' = (if x == 0 then y else pos + 3)                      in Machine program pos' relativeBase
            7 -> let (x, y, z) = getXYZ; newProg = replaceAt (if x < y then 1 else 0) program z  in Machine newProg (pos + 4) relativeBase
            8 -> let (x, y, z) = getXYZ; newProg = replaceAt (if x == y then 1 else 0) program z in Machine newProg (pos + 4) relativeBase
            9 -> let x = getVal' 1                                                               in Machine program (pos + 2) (relativeBase + x)
            _ -> error (show opCode)

data Execution = Completed [Int] | State Machine [Int] [Int] deriving (Show)

process' :: Execution -> Execution
process' (State machine inputStack outputStack) =
    let (Machine program pos relativeBase) = machine
        opCode = (program ! pos)
        opNumber = getOp opCode
    in case opNumber of
        99 -> Completed outputStack
        3 -> if not (null inputStack)
                then
                    let val = head inputStack
                        inpStack = tail inputStack
                        storeLoc = getStoreLoc machine opCode 1
                        newProg = replaceAt val program storeLoc
                        in process' (State (Machine newProg (pos + 2) relativeBase) inpStack outputStack)
                else
                    State machine inputStack outputStack
        4 -> let val = getVal machine opCode 1 in process' (State (Machine program (pos + 2) relativeBase) inputStack (val:outputStack))
        _ -> let machine' = processInst machine in process' (State machine' inputStack outputStack)


process :: UArray Int Int -> Execution
process program = process' (State (Machine program 0 0) [] [])

resume :: Execution -> Int -> Execution
resume (State machine inputStack outputStack) input = process' (State machine (input:inputStack) outputStack)

-- Part 1
data Direction = U | D | L | R deriving (Show)

keyToDir 'w' = U
keyToDir 's' = D
keyToDir 'a' = L
keyToDir 'd' = R
keyToDir _ = R

keyToInput 'w' = 1
keyToInput 's' = 2
keyToInput 'a' = 3
keyToInput 'd' = 4
keyToInput _ = 4

isFinished (State _ _ _) = False
isFinished (Completed _) = True

flushAddInput (State machine _ _) input = State machine [input] []

getOutput (State machine inputStack outputStack) = reverse outputStack
getOutput (Completed outputStack) = reverse outputStack

getNextLocation' (x, y) U = (x, y + 1)
getNextLocation' (x, y) D = (x, y - 1)
getNextLocation' (x, y) L = (x - 1, y)
getNextLocation' (x, y) R = (x + 1, y)

getNextLocation currentLocation _ 0 = currentLocation
getNextLocation loc dir _ = getNextLocation' loc dir

renderTile 0 = '#'
renderTile 1 = 'D'
renderTile 2 = 'S'
renderTile 3 = ' '

toTuple (x, y) t = (x, y, t)

convertTile (Just (x, y, _)) = (x, y, ' ')

addTile tiles (x, y, t) =
    let preExistingAtLoc = find (\(x2, y2, _) -> x == x2 && y == y2) tiles
        preExistingDroid = if t == 'D' then find (\(_, _, t') -> t' == t) tiles else Nothing
        tiles' = if isNothing preExistingAtLoc then tiles else delete (fromJust preExistingAtLoc) tiles
        tiles'' = if isNothing preExistingDroid then tiles' else (convertTile preExistingDroid):delete (fromJust preExistingDroid) tiles'
    in (x, y, t):tiles''

lineToStr :: (Int, Int) -> [(Int, Int, Char)] -> String
lineToStr (minX, maxX) tiles  =
    let linePositions = [minX..maxX]
        pixel x1 = find (\(x2, _, _) -> x1 == x2) tiles
        lineTiles = map pixel linePositions
        pixelOrDefault = maybe '-' (\(_, _, z) -> z)
    in map pixelOrDefault lineTiles

tilesToStrings tiles =
    let ys = map (\(_, y, _) -> y) tiles
        xs = map (\(x, _, _) -> x) tiles
        minY = minimum ys
        maxY = maximum ys
        minX = minimum xs
        maxX = maximum xs
        lines = reverse [minY..maxY]
        getTilesOnLine line = filter (\(_, y, _) -> y == line) tiles
    in map ((lineToStr (minX, maxX)) . getTilesOnLine) lines

printTiles :: [(Int, Int, Char)] -> IO ()
printTiles tiles = mapM_ putStrLn $ tilesToStrings tiles

appendLocation (x, y) seenLocations =
    let alreadySeen = find (\(x2, y2) -> x == x2 && y == y2) seenLocations
    in if isNothing alreadySeen then (x, y):seenLocations else seenLocations

play program =
    go (process' (State (Machine program 0 0) [] [])) (0, 0) Nothing [] []
    where go :: Execution -> (Int, Int) -> Maybe Direction -> [(Int, Int, Char)] -> [(Int, Int)] -> IO ()
          go state currentLocation prevMove tiles seenLocations =
            let nextState = process' state
                output = getOutput nextState
                statusCode = if length output > 0 then head output else -1
                currentLocation' = if isNothing prevMove then currentLocation else getNextLocation currentLocation (fromJust prevMove) statusCode
                newTile = if isNothing prevMove then (0, 0, 'D') else toTuple (getNextLocation' currentLocation (fromJust prevMove)) (renderTile statusCode)
                tiles' = addTile tiles newTile
                seenLocations' = appendLocation currentLocation' seenLocations
            in
                do
                    putStrLn ""
                    printTiles tiles'
                    putStrLn $ "Num moves " <> (show (length seenLocations' - 1))
                    key <- getChar
                    if not (isFinished nextState) then go (flushAddInput nextState (keyToInput key)) currentLocation' (Just (keyToDir key)) tiles' seenLocations' else putStrLn "Done"
