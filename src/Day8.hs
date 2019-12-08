module Day8 where

import           Data.List                      ( elemIndex
                                                , transpose
                                                )
import           Data.Maybe                     ( fromJust )

parseInt :: Char -> Int
parseInt char = read [char] :: Int

parseStr :: String -> [Int]
parseStr str = go str []
  where
    go :: String -> [Int] -> [Int]
    go (x : xs) out = go xs (parseInt x : out)
    go []       out = reverse out

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l | n > 0     = take n l : group n (drop n l)
          | otherwise = error "Negative or zero n"

layerise :: String -> Int -> Int -> [[Int]]
layerise str width height =
    let digitsPerLayer = width * height in group digitsPerLayer $ parseStr str

process str width height =
    let layers            = layerise str width height
        zeroCount         = map (length . filter (== 0)) layers
        smallestZeroCount = minimum zeroCount
        layerIndex        = fromJust $ smallestZeroCount `elemIndex` zeroCount
        layer             = layers !! layerIndex
        oneDigits         = length $ filter (== 1) layer
        twoDigits         = length $ filter (== 2) layer
    in  oneDigits * twoDigits

-- Part 2

process2 str width height =
    let layers          = layerise str width height
        pixels          = transpose layers
        exclTransparent = filter (/= 2)
        colouredPixels  = map exclTransparent pixels
        singlePixels    = map head colouredPixels
        rows            = group width singlePixels
        rowToImage      = map (\x -> if x == 0 then ' ' else '%')
    in  map rowToImage rows

main :: IO ()
main = do
    inpStr <- readFile "./input/Day8.txt"
    print $ process inpStr 25 6
    mapM_ print $ process2 inpStr 25 6
