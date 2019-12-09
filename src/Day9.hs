module Day9 where

import Data.List (find)
import System.IO

instructions :: [Int]
-- instructions = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
-- instructions = [1102,34915192,34915192,7,4,7,99,0]
-- instructions = [104,1125899906842624,99]
instructions = [1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1102,1,3,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1102,533,1,1024,1102,260,1,1023,1101,33,0,1016,1102,37,1,1017,1102,1,36,1009,1101,0,35,1011,1101,0,27,1004,1101,0,0,1020,1101,242,0,1029,1101,0,31,1018,1101,0,38,1007,1101,0,29,1015,1102,1,23,1006,1101,25,0,1002,1102,1,39,1008,1101,0,20,1001,1102,1,34,1012,1102,370,1,1027,1101,30,0,1010,1102,24,1,1014,1101,21,0,1000,1101,22,0,1003,1102,1,26,1005,1101,0,267,1022,1101,1,0,1021,1101,28,0,1013,1101,0,32,1019,1101,251,0,1028,1101,377,0,1026,1102,1,524,1025,109,4,2102,1,-4,63,1008,63,21,63,1005,63,203,4,187,1105,1,207,1001,64,1,64,1002,64,2,64,109,6,1201,-1,0,63,1008,63,36,63,1005,63,229,4,213,1105,1,233,1001,64,1,64,1002,64,2,64,109,18,2106,0,0,4,239,1001,64,1,64,1106,0,251,1002,64,2,64,109,-4,2105,1,-1,1001,64,1,64,1105,1,269,4,257,1002,64,2,64,109,-6,1205,3,287,4,275,1001,64,1,64,1106,0,287,1002,64,2,64,109,-19,1202,9,1,63,1008,63,41,63,1005,63,307,1105,1,313,4,293,1001,64,1,64,1002,64,2,64,109,8,2108,23,-1,63,1005,63,331,4,319,1106,0,335,1001,64,1,64,1002,64,2,64,109,-3,21101,40,0,10,1008,1014,40,63,1005,63,361,4,341,1001,64,1,64,1106,0,361,1002,64,2,64,109,28,2106,0,-5,1001,64,1,64,1106,0,379,4,367,1002,64,2,64,109,-30,1208,7,36,63,1005,63,401,4,385,1001,64,1,64,1105,1,401,1002,64,2,64,109,-1,2101,0,6,63,1008,63,38,63,1005,63,427,4,407,1001,64,1,64,1105,1,427,1002,64,2,64,109,7,1207,-3,27,63,1005,63,445,4,433,1106,0,449,1001,64,1,64,1002,64,2,64,109,8,21107,41,40,0,1005,1016,465,1106,0,471,4,455,1001,64,1,64,1002,64,2,64,109,6,21107,42,43,-6,1005,1016,489,4,477,1105,1,493,1001,64,1,64,1002,64,2,64,109,-26,1208,8,28,63,1005,63,513,1001,64,1,64,1105,1,515,4,499,1002,64,2,64,109,29,2105,1,-1,4,521,1001,64,1,64,1105,1,533,1002,64,2,64,109,-16,1201,-4,0,63,1008,63,23,63,1005,63,553,1105,1,559,4,539,1001,64,1,64,1002,64,2,64,109,4,21101,43,0,-3,1008,1010,41,63,1005,63,579,1106,0,585,4,565,1001,64,1,64,1002,64,2,64,109,-8,1207,-3,24,63,1005,63,605,1001,64,1,64,1106,0,607,4,591,1002,64,2,64,109,1,2102,1,-2,63,1008,63,25,63,1005,63,627,1106,0,633,4,613,1001,64,1,64,1002,64,2,64,109,4,2108,25,-7,63,1005,63,653,1001,64,1,64,1106,0,655,4,639,1002,64,2,64,109,16,21102,44,1,-8,1008,1018,44,63,1005,63,681,4,661,1001,64,1,64,1106,0,681,1002,64,2,64,109,-32,1202,9,1,63,1008,63,22,63,1005,63,703,4,687,1105,1,707,1001,64,1,64,1002,64,2,64,109,1,2107,26,9,63,1005,63,725,4,713,1105,1,729,1001,64,1,64,1002,64,2,64,109,21,1206,5,745,1001,64,1,64,1106,0,747,4,735,1002,64,2,64,109,3,1205,1,763,1001,64,1,64,1106,0,765,4,753,1002,64,2,64,109,-18,2101,0,5,63,1008,63,24,63,1005,63,785,1105,1,791,4,771,1001,64,1,64,1002,64,2,64,109,6,21102,45,1,4,1008,1011,48,63,1005,63,811,1106,0,817,4,797,1001,64,1,64,1002,64,2,64,109,5,21108,46,46,1,1005,1013,835,4,823,1106,0,839,1001,64,1,64,1002,64,2,64,109,-5,21108,47,45,8,1005,1015,855,1105,1,861,4,845,1001,64,1,64,1002,64,2,64,109,9,1206,4,875,4,867,1105,1,879,1001,64,1,64,1002,64,2,64,109,-7,2107,23,-6,63,1005,63,895,1106,0,901,4,885,1001,64,1,64,4,64,99,21101,27,0,1,21101,915,0,0,1106,0,922,21201,1,51547,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21101,942,0,0,1106,0,922,22102,1,1,-1,21201,-2,-3,1,21102,1,957,0,1106,0,922,22201,1,-1,-2,1106,0,968,21202,-2,1,-2,109,-3,2105,1,0]

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

readAInt :: IO Int
readAInt = readLn

process' :: Machine -> IO [Int]
process' machine =
    let (Machine program pos relativeBase) = machine
        opCode = (program !!! pos)
        opNumber = getOp opCode
    in case opNumber of
        99 -> return program
        3 -> do
                putStrLn "Please provide input"
                val <- readAInt
                let storeLoc = getStoreLoc machine opCode 1
                    newProg = replaceAt val program storeLoc
                 in process' (Machine newProg (pos + 2) relativeBase)
        4 -> do 
                print (getVal machine opCode 1)
                process' (Machine program (pos + 2) relativeBase)
        _ -> let machine' = processInst machine in process' machine'


process :: [Int] -> IO [Int]
process program = process' (Machine program 0 0)