{-# LANGUAGE BangPatterns #-}
module Day16 where

import           Data.List                      ( foldl' )
import           Data.Array.Unboxed

basePattern :: [Int]
basePattern = [0, 1, 0, -1]

parseInput :: String -> [Int]
parseInput = map (\x -> read [x])

rep :: [Int] -> Int -> [Int]
rep []       num = []
rep (x : xs) num = replicate num x <> rep xs num

-- starting from num = 1
getRow :: Int -> [Int]
getRow num = (tail . cycle . (rep basePattern)) num

getLast x = abs (x `rem` 10)

-- multiply :: [Int] -> Int -> [Int]
-- multiply input num =
--     let pattern = getRow num
--         go (x : xs) (y : ys) = (x * y) : go xs ys
--         go []       _        = []
--     in  getLast $ sum $ go input pattern

row x = [x, x, x, x]

multiply input num =
    let pattern = row num in getLast $ proc' input pattern True

proc' :: [Int] -> [Int] -> Bool -> Int
proc' [] _ _ = 0
proc' input [d1, d2, d3, d4] firstIteration =
    let inp1              = drop (if firstIteration then d1 - 1 else d1) input
        (toSum, inp2)     = splitAt d2 inp1
        pos               = sum toSum
        inp3              = drop d3 inp2
        (toSumNegs, inp4) = splitAt d4 inp3
        negs              = (-1) * sum toSumNegs
        accum             = pos + negs
    in  accum + proc' inp4 [d1, d2, d3, d4] False


fft inp =
    let len  = length inp
        rows = map (multiply inp) [1 .. len]
    in  rows

process input iterations =
    let inp = parseInput input
        go 0 acc = acc
        go n acc = go (n - 1) (fft acc)
    in  go iterations inp

input =
    "59777373021222668798567802133413782890274127408951008331683345339720122013163879481781852674593848286028433137581106040070180511336025315315369547131580038526194150218831127263644386363628622199185841104247623145887820143701071873153011065972442452025467973447978624444986367369085768018787980626750934504101482547056919570684842729787289242525006400060674651940042434098846610282467529145541099887483212980780487291529289272553959088376601234595002785156490486989001949079476624795253075315137318482050376680864528864825100553140541159684922903401852101186028076448661695003394491692419964366860565639600430440581147085634507417621986668549233797848"
parsedInput = parseInput input
totalLen = 10000 * length parsedInput
lengthenedInput = take totalLen $ cycle parsedInput
skipAmount = (read (take 7 input) :: Int) + 1
fromEnd = totalLen - skipAmount

toString []       = []
toString (x : xs) = show x <> toString xs

arr :: UArray Int Int
arr = listArray (0, totalLen - 1) lengthenedInput

getFromEnd arr index = arr ! ((snd (bounds arr)) - index)

nextPhase arr []       0 = getFromEnd arr 0
nextPhase arr (x : xs) n = (x + getFromEnd arr n) `mod` 10
nextPhase arr a        b = error $ "Error" <> (show a) <> " " <> (show b)

calc :: Int -> UArray Int Int -> UArray Int Int
calc fromEnd arr =
    let newList = foldl' (\a b -> nextPhase arr a b : a) [] [0 .. fromEnd]
        newArr :: UArray Int Int
        newArr = newList `seq` listArray (0, (length newList) - 1) newList
    in  newArr

calcLoop fromEnd list = elems $ go fromEnd arr 100
  where
    arr :: UArray Int Int
    arr = listArray (0, (length list) - 1) list
    go fromEnd arr 0 = arr
    go fromEnd arr n =
        let z' = (calc fromEnd arr) in z' `seq` go fromEnd z' (n - 1)

answer = take 8 $ calcLoop fromEnd lengthenedInput
