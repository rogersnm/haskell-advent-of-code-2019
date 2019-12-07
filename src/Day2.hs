module Day2 where

import Data.List (find)
    
output = 1
restOfProgram = [3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,6,19,1,5,19,23,1,23,6,27,1,5,27,31,1,31,6,35,1,9,35,39,2,10,39,43,1,43,6,47,2,6,47,51,1,5,51,55,1,55,13,59,1,59,10,63,2,10,63,67,1,9,67,71,2,6,71,75,1,5,75,79,2,79,13,83,1,83,5,87,1,87,9,91,1,5,91,95,1,5,95,99,1,99,13,103,1,10,103,107,1,107,9,111,1,6,111,115,2,115,13,119,1,10,119,123,2,123,6,127,1,5,127,131,1,5,131,135,1,135,6,139,2,139,10,143,2,143,9,147,1,147,6,151,1,151,13,155,2,155,9,159,1,6,159,163,1,5,163,167,1,5,167,171,1,10,171,175,1,13,175,179,1,179,2,183,1,9,183,0,99,2,14,0,0]

createMem :: Int -> Int -> [Int]
createMem x y = output : x : y : restOfProgram

instructions = [ createMem x y | x <- [0..99], y <- [0..99] ]

replaceAt :: a -> [a] -> Int -> [a]
replaceAt x (y:ys) z
    | z == 0    = x:ys
    | otherwise = y : replaceAt x ys (z - 1)

processOp1Or2 :: [Int] -> Int -> (Int -> Int -> Int) -> ([Int], Int)
processOp1Or2 program pos op =
    let x = program !! (pos + 1); y = program !! (pos + 2); z = program !! (pos + 3); newProg = replaceAt ((program !! x) `op` (program !! y)) program z in (newProg, 4)
    
processInst :: [Int] -> Int -> Int -> ([Int], Int)
processInst program 1 pos = processOp1Or2 program pos (+)
processInst program 2 pos = processOp1Or2 program pos (*)

process' :: [Int] -> Int -> [Int]
process' program pos
    | program !! pos == 99 = program
    | otherwise = let (prog', step) = processInst program (program !! pos) pos in process' prog' (pos + step)

process :: [Int] -> [Int]
process program = process' program 0

processedPrograms :: [[Int]]
processedPrograms = map process instructions

prog :: Maybe [Int]
prog = find (\x -> head x == 19690720) processedPrograms 