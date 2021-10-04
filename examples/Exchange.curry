-- Curry's solution to the "exchange" problem of the
-- Prolog programming contest at JICSLP'98 in Manchester, see
-- https://people.cs.kuleuven.be/bart.demoen/PrologProgrammingContests/jicslp98competition.ps

import Control.AllValues

-- Prolog-like solution:
exchange :: [Int] -> Int
exchange l = maxList (allValues (value (swap l)))

-- basic value of a list of integers (alternating sum and difference):
value :: [Int] -> Int
value []       = 0
value [x]      = x
value (x:y:xs) = x - y + (value xs)

-- possible swappings of the integer list:
swap :: [Int] -> [Int]
swap []       = []
swap (x:xs)   = x : swap xs
swap (x:y:xs) = (swapValue y) : (swapValue x) : swap xs

swapValue :: Int -> Int
swapValue x = string2PosInt (reverse (posInt2String x))

maxList :: Ord a => [a] -> a
maxList [x]       = x
maxList (x:y:xs) = max x (maxList (y:xs))

posInt2String :: Int -> String
posInt2String n =
   if n<=9 then [chr(ord '0' + n)]
           else posInt2String (n `div` 10) ++ [chr(ord '0' + n `mod` 10)]

string2PosInt :: String -> Int
string2PosInt s = string2PosIntPrefix s 0

string2PosIntPrefix :: String -> Int -> Int
string2PosIntPrefix []     n = n
string2PosIntPrefix (c:cs) n =
  if oc>=ord '0' && oc<=ord '9'
    then string2PosIntPrefix cs (n*10+(ord c)-(ord '0'))
    else 0
 where oc = ord c

goal1 = exchange [1,2]
---> 1

goal2 = exchange [12,56,34]
--> 78
