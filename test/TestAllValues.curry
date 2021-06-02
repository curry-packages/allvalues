-- Some test for module Control.AllSolutions
{-# OPTIONS_FRONTEND -Wno-overlapping #-}

import Test.Prop

import Control.AllValues

-- The famous non-deterministic function:
coin :: Int
coin = 0
coin = 1

-- Test for getAllSolutions
testGetAllSolutions :: PropIO
testGetAllSolutions = getAllSolutions (=:= (coin+coin)) `returns` [0,1,1,2]

-- Test for getAllValues
testGetAllValues :: PropIO
testGetAllValues = getAllValues (coin+coin) `returns` [0,1,1,2]

-- Test for getOneValue
testGetOneValue :: PropIO
testGetOneValue = getOneValue (coin+coin) `returns` Just 0

-- An application of getAllFailures:
--
-- Place n queens on a chessboard so that no queen can capture another queen:
-- (this solution is due to Sergio Antoy)

queens :: Int -> IO [[Int]]
queens n = getAllFailures (permute [1..n]) capture
 where
  capture y = let l1,l2,l3,y1,y2 free in
    l1 ++ [y1] ++ l2 ++ [y2] ++ l3 =:= y & abs (y1-y2) =:= length l2 + 1

  permute []     = []
  permute (x:xs) = ndinsert (permute xs)
   where ndinsert ys     = x : ys
         ndinsert (y:ys) = y : ndinsert ys


testQueens1 :: PropIO
testQueens1 = queens 4 `returns` [[3,1,4,2],[2,4,1,3]]

testQueens2 :: PropIO
testQueens2 = (queens 5 >>= return . length) `returns` 10
