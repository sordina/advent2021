{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Advent09 where

{-

--- Day 9: Smoke Basin ---

These caves seem to be lava tubes. Parts are even still volcanically active; small hydrothermal vents release smoke into the caves that slowly settles like rain.

If you can model how the smoke flows through the caves, you might be able to avoid it and be that much safer. The submarine generates a heightmap of the floor of the nearby caves for you (your puzzle input).

Smoke flows to the lowest point of the area it's in. For example, consider the following heightmap:

2199943210
3987894921
9856789892
8767896789
9899965678

Each number corresponds to the height of a particular location, where 9 is the highest and 0 is the lowest a location can be.

Your first goal is to find the low points - the locations that are lower than any of its adjacent locations. Most locations have four adjacent locations (up, down, left, and right); locations on the edge or corner of the map have three or two adjacent locations, respectively. (Diagonal locations do not count as adjacent.)

In the above example, there are four low points, all highlighted: two are in the first row (a 1 and a 0), one is in the third row (a 5), and one is in the bottom row (also a 5). All other locations on the heightmap have some lower adjacent location, and so are not low points.

The risk level of a low point is 1 plus its height. In the above example, the risk levels of the low points are 2, 1, 6, and 6. The sum of the risk levels of all low points in the heightmap is therefore 15.

Find all of the low points on your heightmap. What is the sum of the risk levels of all low points on your heightmap?

--- Part Two ---

Next, you need to find the largest basins so you know what areas are most important to avoid.

A basin is all locations that eventually flow downward to a single low point. Therefore, every low point has a basin, although some basins are very small. Locations of height 9 do not count as being in any basin, and all other locations will always be part of exactly one basin.

The size of a basin is the number of locations within the basin, including the low point. The example above has four basins.

The top-left basin, size 3:

2199943210
3987894921
9856789892
8767896789
9899965678

The top-right basin, size 9:

2199943210
3987894921
9856789892
8767896789
9899965678

The middle basin, size 14:

2199943210
3987894921
9856789892
8767896789
9899965678

The bottom-right basin, size 9:

2199943210
3987894921
9856789892
8767896789
9899965678

Find the three largest basins and multiply their sizes together. In the above example, this is 9 * 14 * 9 = 1134.

What do you get if you multiply together the sizes of the three largest basins?

-}

import Debug.Trace (traceShow)
import Text.RawString.QQ (r)
import qualified Data.Map as Map
import Control.Arrow ((&&&))
import Data.Maybe (catMaybes, mapMaybe)
import Data.List (sortBy, sortOn)
import qualified Data.Set as Set

-- | Testing day9
-- >>> day9 testInput 
-- 15

day9 :: String -> Int
day9 = sum . map risk . lowPoints . (extents &&& parseInput) . filter (not . null) . lines

extents :: [String] -> (Int, Int)
extents = pred . head . map length &&& pred . length

lowPoints :: ((Int, Int), Map.Map (Int, Int) Int) -> [Int]
lowPoints ((w,h), m) = [ p | x <- [0..w], y <- [0..h], Just p <- [Map.lookup (x,y) m], a <- [around (x,y) m], all (p<) a  ]

around :: (Int, Int) -> Map.Map (Int, Int) Int -> [Int]
around (x,y) m = catMaybes [ Map.lookup (succ x,y) m, Map.lookup (pred x,y) m, Map.lookup (x, succ y) m, Map.lookup (x, pred y) m ]

risk :: Int -> Int
risk = succ

parseInput :: [String] -> Map.Map (Int, Int) Int
parseInput ls = Map.fromList [ ((x,y), read [c]) | (y,l) <- zip [0..] ls, (x,c) <- zip [0..] l]

-- Part 2

-- | Testing day9b
-- >>> day9b testInput
-- 1134

-- | Testing extents
-- >>> (extents &&& ((maximum . map (fst.fst) &&& maximum . map (snd.fst)) . Map.toList) . parseInput) . filter (not . null) . lines $ testInput
-- ((9,4),(9,4))

day9b :: String -> Int
day9b = product . take 3 . sortOn negate . map length . basins . (snd &&& lowLocations) . (extents &&& parseInput) . filter (not . null) . lines

day9d :: String -> String
day9d = drawBasins . ((fst &&& snd) &&& Set.unions . basins . (snd &&& lowLocations)) . (extents &&& parseInput) . filter (not . null) . lines

-- testing drawBasins
-- >>> putStrLn $ "\n" ++ day9d testInput
-- X

lowLocations :: ((Int, Int), Map.Map (Int, Int) Int) -> [(Int,Int)]
lowLocations ((w,h), m) = [ (x,y) | x <- [0..w], y <- [0..h], Just p <- [Map.lookup (x,y) m], a <- [around (x,y) m], all (p<) a  ]

basins :: (Map.Map (Int, Int) Int, [(Int,Int)]) -> [Set.Set (Int,Int)]
basins (m, ls) = map (fixEq (adjacents m) . Set.singleton) ls

adjacents :: Map.Map (Int, Int) Int -> Set.Set (Int, Int) -> Set.Set (Int, Int)
adjacents m = Set.fromList . concatMap (map fst . adjacent m) . Set.toList

drawBasins :: (((Int, Int), Map.Map (Int, Int) Int), Set.Set (Int,Int)) -> String
drawBasins (((w,h), m), bs) = unlines $ [concatMap (drawP y) [0..w]| y <- [0..h]]
  where
  drawP y x
    | Set.member (x,y) bs = "." --  maybe "#" show $ Map.lookup (x,y) m
    | otherwise = maybe "#" show $ Map.lookup (x,y) m

fixEq :: Eq t => (t -> t) -> t -> t
fixEq f x
  | x == y = x
  | otherwise = fixEq f y
  where
  y = f x

adjacent :: Map.Map (Int,Int) Int -> (Int,Int) -> [((Int,Int), Int)]
adjacent m (x,y) = filter ((<9) . snd) $ mapMaybe lkp [(x,y), (succ x,y) , (pred x,y) , (x, succ y) , (x, pred y) ]
  where
  lkp :: (Int, Int) -> Maybe ((Int, Int), Int)
  lkp p = (p,) <$> Map.lookup p m

-- Data

testInput :: String
testInput = [r|
2199943210
3987894921
9856789892
8767896789
9899965678
|]
