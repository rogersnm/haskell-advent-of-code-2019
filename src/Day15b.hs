module Day15b where

import           Data.List                      ( nub
                                                , find
                                                , elemIndex
                                                )
import           Data.Maybe                     ( fromJust )

splitOn :: Char -> String -> [String]
splitOn char s =
    let p = (== char)
    in  case dropWhile p s of
            "" -> []
            s' -> w : splitOn char s'' where (w, s'') = break p s'

lineToCoords _ [] = []
lineToCoords (x0, y0) (x : xs) =
    let rest = lineToCoords (x0 + 1, y0) xs
    in  if x == ' ' then (x0, y0) : rest else rest

linesToCoords lines = go lines 0
  where
    go []       offset = []
    go (x : xs) offset = lineToCoords (0, offset) x <> go xs (offset + 1)

isAdjacent :: (Int, Int) -> (Int, Int) -> Bool
isAdjacent (x1, y1) (x2, y2) =
    (x1 == x2 && ((y1 == (y2 - 1)) || (y1 == (y2 + 1))))
        || (y1 == y2 && ((x1 == (x2 - 1)) || (x1 == (x2 + 1))))


adjCoords :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
adjCoords availableCoords oxygenCoord =
    filter (isAdjacent oxygenCoord) availableCoords

adjacentCoords availableCoords oxygenCoords =
    let go :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
        go availableCoords [] = []
        go availableCoords (x : xs) =
                adjCoords availableCoords x <> go availableCoords xs
        adjacentCoords = nub $ go availableCoords oxygenCoords
        subtractCoords []       adjacentCoords = []
        subtractCoords (x : xs) adjacentCoords = if x `elem` adjacentCoords
            then subtractCoords xs adjacentCoords
            else x : subtractCoords xs adjacentCoords
        oxygenCoords'    = adjacentCoords <> oxygenCoords
        availableCoords' = subtractCoords availableCoords adjacentCoords
    in  (availableCoords', oxygenCoords')

disperse availableCoords oxygenCoord = go availableCoords [oxygenCoord] 0
  where
    go [] oxygenCoords numSteps = numSteps
    go availableCoords oxygenCoords numSteps =
        let (availableCoords', oxygenCoords') =
                    adjacentCoords availableCoords oxygenCoords
        in  go availableCoords' oxygenCoords' (numSteps + 1)

process str =
    let lines      = splitOn '\n' str
        oxygenLine = fromJust $ find (\line -> 'O' `elem` line) lines
        oxygenY    = fromJust $ oxygenLine `elemIndex` lines
        oxygenX    = fromJust $ 'O' `elemIndex` oxygenLine
        coords     = linesToCoords lines
    in  disperse coords (oxygenX, oxygenY)

main :: IO ()
main = do
    inpStr <- readFile "./input/Day15.txt"
    print $ process inpStr
