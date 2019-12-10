module Day10 where

import           Data.List                      ( sort )

splitOn :: Char -> String -> [String]
splitOn char s =
    let p = (== char)
    in  case dropWhile p s of
            "" -> []
            s' -> w : splitOn char s'' where (w, s'') = break p s'

lineToCoords _ [] = []
lineToCoords (x0, y0) (x : xs) =
    let rest = lineToCoords (x0 + 1, y0) xs
    in  if x == '#' then (x0, y0) : rest else rest

linesToCoords lines = go lines 0
  where
    go []       offset = []
    go (x : xs) offset = lineToCoords (0, offset) x <> go xs (offset + 1)

takeOut index list = (list !! index, take index list <> drop (index + 1) list)

processCoords coords = go coords 0
  where
    go coords index
        | index < length coords
        = let (element, remainder) = takeOut index coords
          in  (processCoord element remainder) : go coords (index + 1)
        | otherwise
        = []

processCoord coord surroundings = (coord, go coord surroundings 0)
  where
    go coord surroundings index
        | index < length surroundings
        = let (element, remainder) = takeOut index surroundings
              canSee               = (processPair coord element remainder)
              recurse              = go coord surroundings (index + 1)
          in  if canSee then element : recurse else recurse
        | otherwise
        = []

distance :: (Double, Double) -> (Double, Double) -> Double
distance (x1, y1) (x2, y2) = sqrt (((x2 - x1) ** 2) + ((y2 - y1) ** 2))
dist3 :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
dist3 p1 p2 p3 = (distance p1 p3) + (distance p2 p3)

processPair (x1, y1) (x2, y2) surroundings =
    let dist   = distance (x1, y1) (x2, y2)
        onLine = filter
            (\coord -> (dist3 (x1, y1) (x2, y2) coord) - dist < 1e-12)
            surroundings
    in  null onLine

best (x : xs) = go xs x
  where
    go ((coord', val') : ys) (coord, val) = go
        ys
        (if length val' > length val then (coord', val') else (coord, val))
    go [] winner = winner

process str =
    let lines         = splitOn '\n' str
        width         = length $ head lines
        height        = length lines
        coords        = linesToCoords lines
        result        = processCoords coords
        (coord, list) = best result
        angles        = map (\x -> ((angle coord x), distance coord x, x)) list
        sorted        = sort angles
    in  sorted !! 199

angle (x1, y1) (x2, y2) =
    let delta_x       = x2 - x1
        delta_y       = y2 - y1
        theta_radians = atan2 delta_y delta_x
        adjusted      = theta_radians + (pi / 2)
        adjusted'     = if adjusted < 0 then adjusted + (pi * 2) else adjusted
    in  adjusted'

main :: IO ()
main = do
    inpStr <- readFile "./input/Day10.txt"
    print $ process inpStr
