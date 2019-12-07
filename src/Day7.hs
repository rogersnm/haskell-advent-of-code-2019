module Day7 where

import Data.List (find, permutations)
import System.IO

instructions :: [Int]
instructions = [3,8,1001,8,10,8,105,1,0,0,21,34,47,72,81,102,183,264,345,426,99999,3,9,102,5,9,9,1001,9,3,9,4,9,99,3,9,101,4,9,9,1002,9,3,9,4,9,99,3,9,102,3,9,9,101,2,9,9,102,5,9,9,1001,9,3,9,1002,9,4,9,4,9,99,3,9,101,5,9,9,4,9,99,3,9,101,3,9,9,1002,9,5,9,101,4,9,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99]
-- instructions = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
-- instructions = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
-- instructions = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
-- instructions = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
-- instructions = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]

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

data Execution = Completed [Int] | State [Int] Int [Int] [Int] deriving (Show)

process' :: [Int] -> Int -> [Int] -> [Int] -> Execution
process' program pos inputQueue outputStack
    | program !! pos == 99 = Completed outputStack
    | program !! pos == 3 =
        if not (null inputQueue)
            then
                let val = head inputQueue;
                    inpQueue = tail inputQueue;
                    x = program !! (pos + 1);
                    newProg = replaceAt val program x
                in process' newProg (pos + 2) inpQueue outputStack
            else
                State program pos inputQueue outputStack
    | getOp (program !! pos) == 4 =
        let
            paramModes = getParamModesFromOpCode (program !! pos);
            paramMode = paramModes !! 2;
            val = if paramMode == 0 then program !! (program !! (pos + 1)) else program !! (pos + 1)
          in process' program (pos + 2) inputQueue (val:outputStack)
    | otherwise = let (prog', ic) = processInst program (program !! pos) pos in process' prog' ic inputQueue outputStack

process :: Execution -> Execution
process (State program pos inputQueue outputStack) = process' program pos inputQueue outputStack

fromCompletion (Completed outputStack) = outputStack

chain :: [Int] -> [Int] -> Int
chain instructions phaseSettings =
    let
        ampAOutput = head $ fromCompletion $ process' instructions 0 [head phaseSettings, 0] [];
        ampBOutput = head $ fromCompletion $ process' instructions 0 [phaseSettings !! 1, ampAOutput] [];
        ampCOutput = head $ fromCompletion $ process' instructions 0 [phaseSettings !! 2, ampBOutput] [];
        ampDOutput = head $ fromCompletion $ process' instructions 0 [phaseSettings !! 3, ampCOutput] [];
        ampEOutput = head $ fromCompletion $ process' instructions 0 [phaseSettings !! 4, ampDOutput] []
      in ampEOutput

phaseSettingOptions = permutations [0,1,2,3,4]

inputAndOutput f input = (input, f input)

maximumPhase :: [([Int], Int)] -> ([Int], Int)
maximumPhase options =
    go options (head options)
    where go ((input, output):xs) (maxInp, maxOut) = if output > maxOut then go xs (input, output) else go xs (maxInp, maxOut)
          go [] answer = answer

executePhases = map (inputAndOutput (chain instructions)) phaseSettingOptions
answer = maximumPhase executePhases
-- ([3,1,4,2,0],92663)


-- Part 2
fromState (State _ _ _ outputStack) = outputStack
fromExecution (State _ _ _ outputStack) = outputStack
fromExecution (Completed outputStack) = outputStack

chainFeedback :: [Int] -> [Int] -> (Execution, Execution, Execution, Execution, Execution)
chainFeedback instructions phaseSettings =
    let
        ampAState = process (State instructions 0 [head phaseSettings, 0] []);
        ampBState = process (State instructions 0 [phaseSettings !! 1, head $ fromState ampAState] []);
        ampCState = process (State instructions 0 [phaseSettings !! 2, head $ fromState ampBState] []);
        ampDState = process (State instructions 0 [phaseSettings !! 3, head $ fromState ampCState] []);
        ampEState = process (State instructions 0 [phaseSettings !! 4, head $ fromState ampDState] [])
      in (ampAState, ampBState, ampCState, ampDState, ampEState)

addInput input (State program pos inputQueue outputStack) = State program pos (input:inputQueue) outputStack

chainFeedback' (_, _, _, _, Completed outputStack) = head outputStack
chainFeedback' (ampAState, ampBState, ampCState, ampDState, ampEState) =
    let
        ampAState' = process (addInput (head $ fromExecution ampEState) ampAState)
        ampBState' = process (addInput (head $ fromExecution ampAState') ampBState)
        ampCState' = process (addInput (head $ fromExecution ampBState') ampCState)
        ampDState' = process (addInput (head $ fromExecution ampCState') ampDState)
        ampEState' = process (addInput (head $ fromExecution ampDState') ampEState)
      in chainFeedback' (ampAState', ampBState', ampCState', ampDState', ampEState')

output phaseSettings = chainFeedback' $ chainFeedback instructions phaseSettings

executePhases2 = map (inputAndOutput output) (permutations [5,6,7,8,9])
answer2 = maximumPhase executePhases2
--([7,8,6,9,5],14365052)