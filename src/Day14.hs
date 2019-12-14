module Day14 where

import           Data.List                      ( find
                                                , delete
                                                )
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , isJust
                                                )

splitOn :: Char -> String -> [String]
splitOn char s =
    let p = (== char)
    in  case dropWhile p s of
            "" -> []
            s' -> w : splitOn char s'' where (w, s'') = break p s'

splitOnStr :: String -> String -> (String, String)
splitOnStr split str = go str []
  where
    l = length split
    go (x : xs) acc = if take l (x : xs) == split
        then (reverse acc, drop l (x : xs))
        else go xs (x : acc)

data Chemical = Chemical Int String deriving (Show, Eq)
data Reaction = Reaction [Chemical] Chemical deriving (Show, Eq)

parseLine :: String -> Reaction
parseLine line =
    let (inputs, output) = splitOnStr " => " line
    in  Reaction (parseReactionInput inputs) (parseChemical output)

parseReactionInput :: String -> [Chemical]
parseReactionInput inp =
    let elements        = splitOn ',' inp
        trimStart       = dropWhile (\y -> y == ' ')
        trimmedElements = map (parseChemical . trimStart) elements
    in  trimmedElements

parseChemical :: String -> Chemical
parseChemical inp =
    let [amount, name] = splitOn ' ' inp
        amount'        = read amount
    in  Chemical amount' name

parsePuzzle :: String -> [Reaction]
parsePuzzle puzzleInput = map parseLine $ splitOn '\n' puzzleInput

getReactionThatProduces :: String -> [Reaction] -> Reaction
getReactionThatProduces chemName reactions = fromJust
    $ find (\(Reaction _ (Chemical _ name)) -> name == chemName) reactions

mergeChemicals :: [Chemical] -> [Chemical]
mergeChemicals [] = []
mergeChemicals ((Chemical amount name) : xs) =
    let sameChem    = filter (\(Chemical _ n) -> n == name) xs
        otherChem   = filter (\(Chemical _ n) -> n /= name) xs
        amounts     = sum $ map (\(Chemical a _) -> a) sameChem
        totalAmount = amount + amounts
    in  (Chemical totalAmount name) : mergeChemicals otherChem

deductSpares :: [Chemical] -> [Chemical] -> ([Chemical], [Chemical])
deductSpares chemicals spares = go chemicals spares []
  where
    go :: [Chemical] -> [Chemical] -> [Chemical] -> ([Chemical], [Chemical])
    go chemicals [] spares' = (chemicals, spares')
    go chemicals (y : ys) spares' =
        let (Chemical amount name) = y
            inp = find (\(Chemical _ n) -> n == name) chemicals
        in  case inp of
                (Just chem) ->
                    let (Chemical a n) = chem
                    in  if a == amount
                            then go (delete chem chemicals) ys spares'
                            else if a < amount
                                then go
                                    (delete chem chemicals)
                                    ys
                                    ((Chemical (amount - a) name) : spares')
                                else go
                                    ( (Chemical (a - amount) name)
                                    : (delete chem chemicals)
                                    )
                                    ys
                                    spares'
                _ -> go chemicals ys (y : spares')


getFuel finalChem reactions = go finalChem reactions 0 [] []
  where
    go (Chemical amount name) reactions ore spares queue =
        let
            (chemLeft, nextChemicals) =
                getInputs (Chemical amount name) reactions
            spares' = if isJust chemLeft
                then mergeChemicals (fromJust chemLeft : spares)
                else spares
            oreChem = find (\(Chemical _ name) -> name == "ORE") nextChemicals
            (Chemical ore' _) = fromMaybe (Chemical 0 "ORE") oreChem
            nextChemicals' =
                filter (\(Chemical _ name) -> name /= "ORE") nextChemicals
            (nextChemicals'', spares'') = deductSpares nextChemicals' spares'
            ore'' = ore + ore'
            queue' = mergeChemicals (queue <> nextChemicals'')
        in
            if not (null queue')
                then go (head queue') reactions ore'' spares'' (tail queue')
                else (ore'', spares'')

multiplyReaction numTimes (Reaction inp out) = Reaction
    (map (multiplyChemical numTimes) inp)
    (multiplyChemical numTimes out)
multiplyChemical numTimes (Chemical amount name) =
    Chemical (amount * numTimes) name

getInputs :: Chemical -> [Reaction] -> (Maybe Chemical, [Chemical])
getInputs outputChemical reactions =
    let
        (Chemical amountRequired name) = outputChemical
        reaction                       = getReactionThatProduces name reactions
        (Reaction inputs       output) = reaction
        (Chemical amountOutput _     ) = output
        d                              = amountRequired `div` amountOutput
        r                              = amountRequired `mod` amountOutput
        numTimesToRun                  = d + (if r /= 0 then 1 else 0)
        reaction'                      = multiplyReaction numTimesToRun reaction
        (Reaction inputs'       output') = reaction'
        (Chemical amountOutput' _      ) = output'
        amountSpare                    = amountOutput' - amountRequired
        chemLeft                       = if amountSpare > 0
            then Just (Chemical amountSpare name)
            else Nothing
    in
        (chemLeft, inputs')

part1 puzzleInput =
    let puzzle = parsePuzzle puzzleInput in getFuel (Chemical 1 "FUEL") puzzle

part2 puzzleInput = go 4436970 0 -- Magic number done by eye
  where
    puzzle = parsePuzzle puzzleInput
    go num prev =
        let (ore, spares) = getFuel (Chemical num "FUEL") puzzle
        in  do
                print (ore, num)
                if ore < 1000000000000
                    then go (num + 1) ore
                    else print ("Ore consumed", prev, "Answer", num - 1)

main :: IO ()
main = do
    inpStr <- readFile "./input/Day14.txt"
    print $ fst $ part1 inpStr
    part2 inpStr
    print "Done"
