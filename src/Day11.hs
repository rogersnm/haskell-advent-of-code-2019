module Day11 where

import Data.List (find, nub, delete)
import System.IO

instructions :: [Int]
instructions = [3,8,1005,8,311,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,28,1,1104,0,10,1006,0,71,2,1002,5,10,2,1008,5,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,102,1,8,66,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,87,1006,0,97,2,1002,6,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,116,1006,0,95,1,1009,10,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,145,1,1002,19,10,2,1109,7,10,1006,0,18,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,179,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,102,1,8,200,1,1105,14,10,1,1109,14,10,2,1109,11,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,235,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,257,2,101,9,10,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,101,0,8,282,2,1109,19,10,1,105,0,10,101,1,9,9,1007,9,1033,10,1005,10,15,99,109,633,104,0,104,1,21102,937268368140,1,1,21102,328,1,0,1106,0,432,21102,1,932700599052,1,21101,0,339,0,1105,1,432,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,209421601831,1,21102,1,386,0,1106,0,432,21102,235173604443,1,1,21102,1,397,0,1106,0,432,3,10,104,0,104,0,3,10,104,0,104,0,21101,825439855372,0,1,21102,1,420,0,1106,0,432,21101,0,988220907880,1,21102,431,1,0,1106,0,432,99,109,2,22101,0,-1,1,21101,40,0,2,21102,1,463,3,21102,453,1,0,1106,0,496,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,458,459,474,4,0,1001,458,1,458,108,4,458,10,1006,10,490,1102,1,0,458,109,-2,2106,0,0,0,109,4,2102,1,-1,495,1207,-3,0,10,1006,10,513,21102,0,1,-3,22102,1,-3,1,21202,-2,1,2,21102,1,1,3,21101,532,0,0,1105,1,537,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,560,2207,-4,-2,10,1006,10,560,21201,-4,0,-4,1106,0,628,22102,1,-4,1,21201,-3,-1,2,21202,-2,2,3,21102,1,579,0,1106,0,537,21202,1,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,598,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,620,21201,-1,0,1,21102,1,620,0,105,1,495,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0]

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

(!!!) :: [Int] -> Int -> Int
(!!!) list pos = if pos >= length list then 0 else list !! pos

replaceAt :: Int -> [Int] -> Int -> [Int]
replaceAt val (x:xs) pos
    | pos == 0             = val:xs
    | pos >= length (x:xs) =
        let len = length (x:xs)
            blankCount = pos - len
            filler = replicate blankCount (0 :: Int)
        in (x:xs) <> filler <> [val]
    | otherwise = x : replaceAt val xs (pos - 1)

data Machine = Machine [Int] Int Int deriving (Show)

getVal :: Machine -> Int -> Int -> Int
getVal (Machine program pos relativeBase) opCode argNum  =
    let paramModes = getParamModesFromOpCode opCode;
        paramMode = paramModes !! (2 - (argNum - 1))
        arg = program !!! (pos + argNum)
    in
        case paramMode of
            0 -> program !!! arg
            1 -> arg
            2 -> program !!! (relativeBase + arg)

getStoreLoc :: Machine -> Int -> Int -> Int
getStoreLoc (Machine program pos relativeBase) opCode argNum =
    let paramModes = getParamModesFromOpCode opCode;
        paramMode = paramModes !! (2 - (argNum - 1))
        arg = program !! (pos + argNum)
    in
        case paramMode of
            0 -> arg
            2 -> relativeBase + arg

processInst :: Machine -> Machine
processInst machine =
    let (Machine program pos relativeBase) = machine
        opCode = program !!! pos;
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
        opCode = (program !!! pos)
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


process :: [Int] -> Execution
process program = process' (State (Machine program 0 0) [] [])

resume :: Execution -> Int -> Execution
resume (State machine inputStack outputStack) input = process' (State machine (input:inputStack) outputStack)

-- Part 1

data Direction = U | D | L | R deriving (Show)

newDirection :: Direction -> Int -> Direction

-- CCW
newDirection U 0 = L
newDirection L 0 = D
newDirection D 0 = R
newDirection R 0 = U

-- CW
newDirection U 1 = R
newDirection R 1 = D
newDirection D 1 = L
newDirection L 1 = U

newPosition (x, y) U = (x, y + 1)
newPosition (x, y) D = (x, y - 1)
newPosition (x, y) L = (x - 1, y)
newPosition (x, y) R = (x + 1, y)

getOutput (State machine inputStack outputStack) = (take 2 outputStack, (State machine inputStack (drop 2 outputStack)))
getOutput (Completed outputStack) = (take 2 outputStack, Completed (drop 2 outputStack))

addInput (State machine inputStack outputStack) input = State machine (input:inputStack) outputStack

isFinished (State _ _ _) = False
isFinished (Completed _) = True

data Square = Square (Int, Int) Int deriving (Show, Eq)

getSquareColour (Square _ colour) = colour

getColour :: (Int, Int) -> [Square] -> Int
getColour pos squares =
    let squares' = filter (\(Square p _) -> p == pos) squares
    in if null squares' then 0 else getSquareColour (head squares')

setColour :: Square -> [Square] -> [Square]
setColour (Square pos colour) squares =
    let squares' = filter (\(Square p _) -> p == pos) squares
        newSquares = if null squares' then squares else delete (head squares') squares
    in (Square pos colour) : newSquares

go :: (Int, Int) -> Direction -> Execution -> [Square] -> [Square]
go position direction execution squares =
    let colour = getColour position squares
        execution' = addInput execution colour
        nextExecution = process' execution'
        ([turn, newColour], nextExecution') = getOutput nextExecution
        direction' = newDirection direction turn
        position' = newPosition position direction'
        squares' = setColour (Square position newColour) squares
    in  if isFinished nextExecution' then squares' else go position' direction' nextExecution' squares'

initialPos :: (Int, Int)
initialPos = (0, 0)
initialExecution = (State (Machine instructions 0 0) [] [])

squares = go initialPos U initialExecution []
part1 = length squares

-- Part 2

squares2 = go initialPos U initialExecution [(Square initialPos 1)]

xs = map (\(Square (x, _) _) -> x)
ys = map (\(Square (_, y) _) -> y)

minX = minimum $ xs squares2
minY = minimum $ ys squares2

realignedSquares = map (\(Square (x, y) colour)  -> Square (x - minX, y - minY) colour) squares2

whiteSquares = filter (\(Square _ colour) -> colour == 1) realignedSquares

width = maximum $ xs whiteSquares
height = maximum $ ys whiteSquares

whitePixels = map (\(Square pos _) -> pos) whiteSquares

lineToStr :: [(Int, Int)] -> Int -> String
lineToStr whitePixels line =
    let pixels = filter (\(x, y) -> y == line) whitePixels
        linePositions = [ (x, line) | x <- [0..width] ]
        white (x1, y1) = any (\(x2, y2) -> x1 == x2 && y1 == y2) pixels
        lineBool = map white linePositions
        toChar b = if b then '%' else ' '
    in map toChar lineBool

linesToStr whitePixels = map (lineToStr whitePixels) $ reverse [0..height]

printPixels :: IO ()
printPixels = mapM_ print $ linesToStr whitePixels