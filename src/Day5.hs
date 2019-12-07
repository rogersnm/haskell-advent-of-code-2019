module Day5 where

import Data.List (find)
import System.IO

instructions :: [Int]
instructions = [3,225,1,225,6,6,1100,1,238,225,104,0,1102,45,16,225,2,65,191,224,1001,224,-3172,224,4,224,102,8,223,223,1001,224,5,224,1,223,224,223,1102,90,55,225,101,77,143,224,101,-127,224,224,4,224,102,8,223,223,1001,224,7,224,1,223,224,223,1102,52,6,225,1101,65,90,225,1102,75,58,225,1102,53,17,224,1001,224,-901,224,4,224,1002,223,8,223,1001,224,3,224,1,224,223,223,1002,69,79,224,1001,224,-5135,224,4,224,1002,223,8,223,1001,224,5,224,1,224,223,223,102,48,40,224,1001,224,-2640,224,4,224,102,8,223,223,1001,224,1,224,1,224,223,223,1101,50,22,225,1001,218,29,224,101,-119,224,224,4,224,102,8,223,223,1001,224,2,224,1,223,224,223,1101,48,19,224,1001,224,-67,224,4,224,102,8,223,223,1001,224,6,224,1,223,224,223,1101,61,77,225,1,13,74,224,1001,224,-103,224,4,224,1002,223,8,223,101,3,224,224,1,224,223,223,1102,28,90,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,226,677,224,102,2,223,223,1005,224,329,1001,223,1,223,8,226,677,224,1002,223,2,223,1005,224,344,101,1,223,223,8,226,226,224,1002,223,2,223,1006,224,359,101,1,223,223,1008,677,226,224,1002,223,2,223,1005,224,374,1001,223,1,223,108,677,677,224,1002,223,2,223,1005,224,389,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,404,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,419,1001,223,1,223,7,677,226,224,1002,223,2,223,1005,224,434,101,1,223,223,1108,226,226,224,1002,223,2,223,1005,224,449,101,1,223,223,7,226,226,224,102,2,223,223,1005,224,464,101,1,223,223,108,677,226,224,102,2,223,223,1005,224,479,1001,223,1,223,1007,677,226,224,1002,223,2,223,1006,224,494,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,509,1001,223,1,223,107,677,677,224,1002,223,2,223,1005,224,524,101,1,223,223,1108,226,677,224,102,2,223,223,1006,224,539,1001,223,1,223,8,677,226,224,102,2,223,223,1005,224,554,101,1,223,223,1007,226,226,224,102,2,223,223,1006,224,569,1001,223,1,223,107,677,226,224,102,2,223,223,1005,224,584,1001,223,1,223,108,226,226,224,102,2,223,223,1006,224,599,1001,223,1,223,107,226,226,224,1002,223,2,223,1006,224,614,1001,223,1,223,1108,677,226,224,1002,223,2,223,1005,224,629,1001,223,1,223,1107,677,677,224,102,2,223,223,1005,224,644,1001,223,1,223,1008,677,677,224,102,2,223,223,1005,224,659,101,1,223,223,1107,677,226,224,1002,223,2,223,1006,224,674,101,1,223,223,4,223,99,226]

replaceAt :: a -> [a] -> Int -> [a]
replaceAt val (x:xs) pos
    | pos == 0  = val:xs
    | otherwise = x : replaceAt val xs (pos - 1)

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

getVal opcode program pos argNum =
    let paramModes = getParamModesFromOpCode opcode;
        paramMode = paramModes !! (2 - (argNum - 1));
        arg = program !! (pos + argNum)
    in
        if paramMode == 0 then program !! arg else arg

processInst :: [Int] -> Int -> Int -> ([Int], Int)
processInst program opcode pos
    | getOp opcode == 1 = let x = getVal opcode program pos 1; y = getVal opcode program pos 2; z = program !! (pos + 3); newProg = replaceAt (x + y) program z in (newProg, pos + 4)
    | getOp opcode == 2 = let x = getVal opcode program pos 1; y = getVal opcode program pos 2; z = program !! (pos + 3); newProg = replaceAt (x * y) program z in (newProg, pos + 4)
    | getOp opcode == 5 = let x = getVal opcode program pos 1; y = getVal opcode program pos 2 in (program, if x /= 0 then y else pos + 3)
    | getOp opcode == 6 = let x = getVal opcode program pos 1; y = getVal opcode program pos 2 in (program, if x == 0 then y else pos + 3)
    | getOp opcode == 7 = let x = getVal opcode program pos 1; y = getVal opcode program pos 2; z = program !! (pos + 3); newProg = replaceAt (if x < y then 1 else 0) program z in (newProg, pos + 4)
    | getOp opcode == 8 = let x = getVal opcode program pos 1; y = getVal opcode program pos 2; z = program !! (pos + 3); newProg = replaceAt (if x == y then 1 else 0) program z in (newProg, pos + 4)
    | otherwise = error (show opcode)

readAInt :: IO Int
readAInt = readLn

process' :: [Int] -> Int -> IO [Int]
process' program pos
    | program !! pos == 99 = return program
    | program !! pos == 3 = do
        putStrLn "Please provide input"
        val <- readAInt
        let x = program !! (pos + 1); newProg = replaceAt val program x in process' newProg (pos + 2)
    | getOp (program !! pos) == 4 = do
        let paramModes = getParamModesFromOpCode (program !! pos); paramMode = paramModes !! 2 in if paramMode == 0 then print (program !! (program !! (pos + 1))) else print (program !! (pos + 1))
        process' program (pos + 2)
    | otherwise = let (prog', ic) = processInst program (program !! pos) pos in process' prog' (ic)

process :: [Int] -> IO [Int]
process program = process' program 0
