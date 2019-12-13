module Day13 where

import Data.List (find, nub, delete, sort)
import System.IO
import Data.Maybe (fromJust)
import Data.Array.Unboxed

instructionsList :: [Int]
instructionsList = [1,380,379,385,1008,2751,751761,381,1005,381,12,99,109,2752,1101,0,0,383,1101,0,0,382,21001,382,0,1,21002,383,1,2,21101,37,0,0,1106,0,578,4,382,4,383,204,1,1001,382,1,382,1007,382,44,381,1005,381,22,1001,383,1,383,1007,383,24,381,1005,381,18,1006,385,69,99,104,-1,104,0,4,386,3,384,1007,384,0,381,1005,381,94,107,0,384,381,1005,381,108,1105,1,161,107,1,392,381,1006,381,161,1101,0,-1,384,1106,0,119,1007,392,42,381,1006,381,161,1102,1,1,384,20102,1,392,1,21101,22,0,2,21101,0,0,3,21102,1,138,0,1106,0,549,1,392,384,392,20101,0,392,1,21102,22,1,2,21102,3,1,3,21101,0,161,0,1106,0,549,1101,0,0,384,20001,388,390,1,20102,1,389,2,21101,180,0,0,1105,1,578,1206,1,213,1208,1,2,381,1006,381,205,20001,388,390,1,21002,389,1,2,21102,1,205,0,1106,0,393,1002,390,-1,390,1102,1,1,384,20101,0,388,1,20001,389,391,2,21101,0,228,0,1105,1,578,1206,1,261,1208,1,2,381,1006,381,253,21002,388,1,1,20001,389,391,2,21102,253,1,0,1106,0,393,1002,391,-1,391,1102,1,1,384,1005,384,161,20001,388,390,1,20001,389,391,2,21101,0,279,0,1106,0,578,1206,1,316,1208,1,2,381,1006,381,304,20001,388,390,1,20001,389,391,2,21101,304,0,0,1105,1,393,1002,390,-1,390,1002,391,-1,391,1101,1,0,384,1005,384,161,20102,1,388,1,20102,1,389,2,21102,0,1,3,21101,0,338,0,1106,0,549,1,388,390,388,1,389,391,389,21002,388,1,1,20101,0,389,2,21101,4,0,3,21102,1,365,0,1106,0,549,1007,389,23,381,1005,381,75,104,-1,104,0,104,0,99,0,1,0,0,0,0,0,0,376,20,19,1,1,22,109,3,21201,-2,0,1,21202,-1,1,2,21101,0,0,3,21101,0,414,0,1106,0,549,22102,1,-2,1,21201,-1,0,2,21101,429,0,0,1105,1,601,2101,0,1,435,1,386,0,386,104,-1,104,0,4,386,1001,387,-1,387,1005,387,451,99,109,-3,2105,1,0,109,8,22202,-7,-6,-3,22201,-3,-5,-3,21202,-4,64,-2,2207,-3,-2,381,1005,381,492,21202,-2,-1,-1,22201,-3,-1,-3,2207,-3,-2,381,1006,381,481,21202,-4,8,-2,2207,-3,-2,381,1005,381,518,21202,-2,-1,-1,22201,-3,-1,-3,2207,-3,-2,381,1006,381,507,2207,-3,-4,381,1005,381,540,21202,-4,-1,-1,22201,-3,-1,-3,2207,-3,-4,381,1006,381,529,22101,0,-3,-7,109,-8,2105,1,0,109,4,1202,-2,44,566,201,-3,566,566,101,639,566,566,1201,-1,0,0,204,-3,204,-2,204,-1,109,-4,2105,1,0,109,3,1202,-1,44,593,201,-2,593,593,101,639,593,593,21001,0,0,-2,109,-3,2106,0,0,109,3,22102,24,-2,1,22201,1,-1,1,21101,0,541,2,21102,750,1,3,21101,0,1056,4,21102,1,630,0,1105,1,456,21201,1,1695,-2,109,-3,2105,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,2,2,2,2,2,0,2,2,2,0,2,2,2,2,0,0,2,2,2,0,2,2,2,2,2,2,2,0,2,0,2,2,2,2,2,2,0,2,2,0,0,1,1,0,2,2,0,0,2,2,2,2,2,0,0,2,2,0,2,2,0,2,0,2,2,0,0,2,2,0,2,0,2,0,0,2,0,2,2,0,2,0,2,2,0,1,1,0,2,2,2,2,0,0,0,2,0,2,0,0,0,0,2,2,2,2,0,2,2,2,0,0,2,2,2,0,2,2,2,2,2,0,0,2,2,0,0,2,0,1,1,0,2,2,0,2,2,2,0,0,0,2,2,0,2,2,0,2,2,0,0,2,0,2,0,0,2,2,2,2,0,2,2,2,0,2,0,0,2,2,2,2,0,1,1,0,0,0,2,2,0,0,0,0,0,2,0,0,0,2,2,0,0,2,0,0,2,2,0,0,2,2,2,2,0,2,0,0,2,0,2,2,2,0,2,2,0,1,1,0,0,2,2,0,2,2,2,2,2,2,2,2,0,2,2,0,2,2,0,0,2,0,0,0,2,2,0,2,2,0,0,0,0,2,0,2,0,0,0,2,0,1,1,0,2,2,2,0,2,0,2,2,0,2,0,0,2,0,2,2,2,0,2,2,2,2,0,0,0,0,0,2,0,2,2,2,2,0,0,0,2,0,0,0,0,1,1,0,0,0,2,2,2,2,2,2,0,2,0,2,2,0,2,2,2,2,0,0,0,0,2,2,0,0,2,2,2,0,2,0,2,2,0,0,2,2,2,0,0,1,1,0,0,0,0,2,2,2,0,0,2,0,2,2,0,2,2,0,0,0,0,2,0,2,2,2,0,2,2,0,2,2,0,0,2,2,0,2,2,2,0,2,0,1,1,0,0,2,0,0,0,2,2,0,2,0,2,2,0,0,2,0,2,2,2,2,2,2,0,0,2,2,0,0,2,2,2,0,2,2,0,0,0,0,2,2,0,1,1,0,0,2,0,0,0,2,2,2,0,2,0,2,2,2,2,0,2,2,0,0,2,2,2,2,0,2,2,2,2,2,2,0,0,0,0,2,2,2,2,0,0,1,1,0,2,2,2,2,2,0,2,0,0,2,2,0,2,0,2,0,2,2,0,2,0,2,2,2,2,2,0,0,0,0,2,2,2,2,0,0,0,0,0,0,0,1,1,0,2,2,0,2,2,0,0,0,2,2,2,2,0,0,2,0,2,0,2,0,2,2,0,0,2,2,0,2,2,2,0,2,0,0,0,0,2,2,2,2,0,1,1,0,0,2,2,2,2,2,0,2,0,2,2,0,0,0,2,0,2,2,0,0,0,2,2,2,2,2,2,2,2,2,0,0,0,2,0,0,0,0,2,0,0,1,1,0,0,0,0,2,0,0,2,2,2,2,2,2,0,2,0,2,2,2,2,0,0,0,2,2,2,2,0,2,2,0,2,2,0,0,0,2,2,2,2,0,0,1,1,0,0,0,0,2,0,2,2,2,2,2,0,2,2,2,2,2,2,0,0,2,0,2,2,2,2,2,2,0,0,2,0,2,2,2,0,2,2,0,2,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,27,30,18,28,94,18,40,59,97,67,72,28,75,43,73,43,86,45,58,6,22,11,34,64,74,45,90,20,49,72,59,3,65,32,80,39,51,12,22,48,11,98,5,45,80,41,88,83,63,29,65,45,80,53,23,68,27,51,43,98,46,4,98,98,68,64,28,95,5,94,12,52,91,15,35,29,47,3,60,54,75,24,71,67,22,53,77,40,47,41,8,53,38,40,83,25,65,80,44,30,37,57,62,13,74,23,13,22,43,31,76,77,22,21,16,89,16,18,95,31,21,37,66,33,85,38,35,29,47,90,30,95,17,43,58,40,3,41,49,59,96,38,37,73,68,83,92,83,45,25,10,12,51,33,54,53,67,95,46,58,97,19,4,51,5,46,42,49,65,67,88,18,64,84,19,72,9,80,71,34,92,76,62,86,68,19,87,88,42,65,40,91,45,44,56,58,50,53,98,87,62,97,27,60,16,9,19,9,66,85,58,71,42,63,90,24,86,12,37,27,84,87,79,16,4,90,98,13,17,83,87,24,32,19,60,46,77,69,55,38,68,92,36,13,61,27,76,76,73,32,13,78,89,38,93,32,30,66,67,31,39,8,79,57,42,84,11,56,24,77,57,84,80,75,97,69,83,61,69,69,19,22,82,7,35,85,58,88,24,91,91,12,92,28,43,68,8,17,67,65,20,65,72,66,94,93,11,85,27,72,11,26,42,25,83,11,44,53,22,51,32,27,16,67,74,39,37,51,13,78,54,82,14,64,62,49,89,68,60,25,51,32,50,5,28,78,18,26,9,61,41,7,97,48,25,16,29,50,1,40,91,46,87,69,51,72,84,95,20,78,49,20,5,70,30,72,15,36,46,43,81,33,2,67,10,33,68,59,28,46,23,64,23,58,8,14,47,37,82,50,3,63,15,85,24,75,5,35,63,36,17,70,30,35,24,35,81,26,97,24,65,42,26,37,69,67,52,9,79,91,56,53,96,75,78,34,98,47,67,82,34,96,67,75,55,12,56,50,32,56,3,25,38,70,81,24,44,50,69,51,25,66,54,14,71,54,79,88,62,43,92,38,23,61,31,29,78,9,56,2,61,15,58,73,5,97,47,81,84,39,77,81,52,63,30,91,81,74,27,20,98,4,53,32,95,11,13,28,91,97,45,67,12,65,78,41,18,30,98,69,88,58,14,55,42,6,64,14,55,98,22,16,51,4,16,89,96,21,38,2,8,49,70,11,94,34,19,5,98,25,27,42,82,67,80,67,22,78,50,18,67,55,92,61,43,66,11,25,73,53,8,79,38,81,84,60,89,14,33,18,86,78,55,96,92,6,36,64,96,50,64,93,20,3,27,79,98,53,69,77,85,62,68,83,67,71,29,68,52,71,98,31,17,75,9,43,92,39,19,58,97,64,70,58,74,10,37,74,28,35,97,33,21,27,72,72,82,77,91,89,21,52,76,82,24,91,73,31,19,90,97,37,5,88,53,7,20,89,72,20,2,28,61,68,40,17,81,27,92,78,11,30,78,62,98,15,38,7,46,21,48,81,43,1,70,70,26,20,37,91,28,40,81,53,90,54,10,92,88,98,13,94,88,41,66,31,69,45,28,64,77,24,71,11,11,56,93,65,5,57,54,93,7,43,6,96,1,22,36,15,67,88,33,70,14,46,71,12,57,37,80,46,13,53,63,77,61,56,3,12,60,34,77,70,56,57,5,83,38,9,70,32,79,90,85,50,65,5,45,64,29,47,15,2,46,30,13,89,53,19,80,38,63,25,10,46,94,93,86,61,41,22,98,52,81,76,85,34,25,72,26,64,44,52,47,69,21,39,67,35,43,75,21,58,3,15,71,44,77,42,20,67,17,25,12,6,50,2,63,78,41,80,26,19,9,30,36,16,86,63,51,7,29,16,5,94,15,53,26,69,67,21,38,13,65,78,34,94,58,25,33,14,12,57,67,96,18,79,37,64,83,23,59,23,52,13,50,88,98,26,11,85,39,36,47,10,77,4,81,25,6,14,11,45,72,70,94,2,54,23,83,95,58,20,25,15,24,69,35,96,70,93,79,79,5,39,83,43,29,4,64,82,52,16,84,36,89,31,21,90,41,39,23,35,83,65,89,53,6,64,68,55,59,57,17,78,92,6,17,1,84,86,19,78,69,34,12,36,41,60,16,37,24,31,31,91,13,93,38,17,80,25,37,9,49,59,96,80,68,64,40,35,45,10,16,13,23,33,52,63,84,9,93,31,40,70,69,19,22,79,25,20,47,83,40,29,86,96,84,23,31,42,82,87,83,5,70,25,15,23,77,41,31,73,2,3,74,69,44,31,10,96,52,93,88,98,56,11,55,47,34,86,63,7,11,86,77,77,39,75,44,31,58,10,20,1,751761]

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

group [] size = []
group xs size = take size xs : group (drop size xs) size

(Completed outputStack) = process instructions
output = reverse outputStack
tiles = group output 3
blockTiles = filter (\[x, y, id] -> id == 2) tiles
numBlocks = length blockTiles

-- Part 2

getOutput (State machine inputStack outputStack) = reverse outputStack
getOutput (Completed outputStack) = reverse outputStack

renderTile :: Int -> Char
renderTile 0 = ' ' -- Empty
renderTile 1 = '|' -- Wall
renderTile 2 = '#' -- Block
renderTile 3 = 'T' -- Paddle
renderTile 4 = '%' -- Ball
renderTile e = error ("Unknown tile ID: " <> show e)

lineToStr :: [[Int]] -> Int -> String
lineToStr tiles lineNum = 
    let lineTiles = filter (\[x, y, id] -> y == lineNum && x /= -1) tiles
        tileTypes = map (\[x, y, id] -> id) lineTiles
        chars = map renderTile tileTypes
    in chars

linesToStr tiles =
    go tiles 0
    where height = maximum $ map (\[x, y, id] -> y) tiles
          go tiles lineNum
            | lineNum >= height = []
            | otherwise = lineToStr tiles lineNum : go tiles (lineNum + 1)

printPixels :: [[Int]] -> IO ()
printPixels tiles = mapM_ putStrLn $ linesToStr tiles

readAChar :: IO String
readAChar = readLn

flushAddInput (State machine _ _) input = State machine [input] []

isFinished (State _ _ _) = False
isFinished (Completed _) = True

inputToDir 'a' = -1
inputToDir 's' = 0
inputToDir 'd' = 1

updateTiles oldTiles newTiles = 
    let newPositions = map (\[x, y,z] -> (x, y)) newTiles
        sameTiles = filter (\[x, y, z] -> (x, y) `notElem` newPositions) oldTiles
        allTiles = sameTiles <> newTiles
    in sort allTiles

play program =
    go (process' (State (Machine program 0 0) [] [])) []
    where go state tiles =
            let nextState = process' state
                output = getOutput nextState
                newTiles = group output 3
                newScreen = updateTiles tiles newTiles
                score = find (\[x, y, _] -> x == -1 && y == 0) newScreen
                [padPosX, padPosY, _] = fromJust $ find (\[_, _, id] -> id == 3) newScreen
                [ballPosX, ballPosY, _] = fromJust $ find (\[_, _, id] -> id == 4) newScreen
            in
                do
                    printPixels newScreen
                    putStrLn $ ("Score: " <> show score)
                    -- key <- getChar
                    -- let input = inputToDir key in go (flushAddInput nextState input) newScreen
                    if not (isFinished nextState) then let input = signum (ballPosX - padPosX) in go (flushAddInput nextState input) newScreen else putStrLn "Done"

freeGame = replaceAt 2 instructions 0
