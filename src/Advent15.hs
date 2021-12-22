{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NegativeLiterals #-}

module Advent15 where

{-

--- Day 15: Chiton ---

You've almost reached the exit of the cave, but the walls are getting closer together. Your submarine can barely still fit, though; the main problem is that the walls of the cave are covered in chitons, and it would be best not to bump any of them.

The cavern is large, but has a very low ceiling, restricting your motion to two dimensions. The shape of the cavern resembles a square; a quick scan of chiton density produces a map of risk level throughout the cave (your puzzle input). For example:

1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581

You start in the top left position, your destination is the bottom right position, and you cannot move diagonally. The number at each position is its risk level; to determine the total risk of an entire path, add up the risk levels of each position you enter (that is, don't count the risk level of your starting position unless you enter it; leaving it adds no risk to your total).

Your goal is to find a path with the lowest total risk. In this example, a path with the lowest total risk is highlighted here:

1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581

The total risk of this path is 40 (the starting position is never entered, so its risk is not counted).

What is the lowest total risk of any path from the top left to the bottom right?

-}

import Text.RawString.QQ (r)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes)
import Data.Ord (comparing)
import Data.Foldable (minimumBy)
import Utils
import Control.Arrow ((&&&))

-- | Testing day14b
-- >>> day15 testInput
-- 40

day15 :: String -> Int
day15 = fst . solve' . parseInput

solve' :: Map.Map (Int, Int) Int -> (Int, [(Int, Int)])
solve' m = solve edge
  where
  edge = maximum $ Map.keys m
  solutions = Map.fromList $ map (id &&& solve) (crossProductZero edge)
  solve :: (Int,Int) -> (Int, [(Int,Int)])
  solve p@(0,0) = (0, [p])
  solve p@(x,y) = (n+s, p:ps)
    where
    n  = fromMaybe 0 $ Map.lookup p m
    sx = Map.lookup (pred x, y) solutions
    sy = Map.lookup (x, pred y) solutions
    (s,ps) = minimumBy (comparing fst) $ catMaybes [sx,sy]

parseInput :: String -> Map.Map (Int,Int) Int
parseInput s = Map.fromList cs
  where
  ls = lines s
  cs = concat $ zipWith (\ y l -> zipWith (\ x c -> ((x, y), read [c])) [0 .. ] l) [0..] ls

-- | Testing parseInput
-- >>> parseInput testInput
-- fromList [((0,0),1),((0,1),1),((0,2),2),((0,3),3),((0,4),7),((0,5),1),((0,6),1),((0,7),3),((0,8),1),((0,9),2),((1,0),1),((1,1),3),((1,2),1),((1,3),6),((1,4),4),((1,5),3),((1,6),3),((1,7),1),((1,8),2),((1,9),3),((2,0),6),((2,1),8),((2,2),3),((2,3),9),((2,4),6),((2,5),1),((2,6),5),((2,7),2),((2,8),9),((2,9),1),((3,0),3),((3,1),1),((3,2),6),((3,3),4),((3,4),3),((3,5),9),((3,6),9),((3,7),5),((3,8),3),((3,9),1),((4,0),7),((4,1),3),((4,2),5),((4,3),9),((4,4),4),((4,5),1),((4,6),9),((4,7),4),((4,8),1),((4,9),9),((5,0),5),((5,1),7),((5,2),1),((5,3),3),((5,4),1),((5,5),2),((5,6),1),((5,7),2),((5,8),3),((5,9),4),((6,0),1),((6,1),3),((6,2),1),((6,3),1),((6,4),7),((6,5),8),((6,6),2),((6,7),1),((6,8),8),((6,9),4),((7,0),7),((7,1),6),((7,2),3),((7,3),5),((7,4),1),((7,5),1),((7,6),4),((7,7),6),((7,8),5),((7,9),5),((8,0),4),((8,1),7),((8,2),2),((8,3),6),((8,4),1),((8,5),3),((8,6),2),((8,7),3),((8,8),2),((8,9),8),((9,0),2),((9,1),2),((9,2),8),((9,3),9),((9,4),1),((9,5),7),((9,6),1),((9,7),9),((9,8),1),((9,9),1)]

testInput :: String
testInput = unlines $ drop 1 $ lines [r|
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
|]
