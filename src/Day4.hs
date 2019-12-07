module Day4 where

startCode = 402328
endCode = 864247

codes = [startCode..endCode]

intToList :: Integral x => x -> [x]
intToList 0 = []
intToList x = intToList (x `div` 10) ++ [x `mod` 10]

-- hasPair [x] = False
-- hasPair (x:xs) = x == head xs || hasPair xs

-- True if [P, P, x, ...]
startPair (a:b:c:xs) = a == b && b /= c

-- True if [..., x, P, P]
endPair [a,b,c] = a /= b && b == c
endPair (x:xs)  = endPair xs

-- True if [x, P, P, x]
middlePair [a,b,c,d] = a /= b && b == c && c /= d 

-- True if [..., x, P, P, x, ...] anywhere in the list
middlePairs list
  | length list > 4  = middlePair (take 4 list) || middlePairs (tail list)
  | length list == 4 = middlePair list
  | otherwise        = False

hasPair []    = False
hasPair [a]   = False
hasPair [a,b] = a == b
hasPair list  = startPair list || middlePairs list || endPair list

-- hasPair [a,b,c,d,e,f] = a == b && b /= c ||
--                         a /= b && b == c && c /= d ||
--                         b /= c && c == d && d /= e ||
--                         c /= d && d == e && e /= f ||
--                         d /= e && e == f

neverDecreases [x] = True
neverDecreases (x:xs) = head xs >= x && neverDecreases xs

hasPairAndNeverDecreases x = hasPair x && neverDecreases x

numbers = filter (hasPairAndNeverDecreases . intToList) codes