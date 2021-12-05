{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Advent05 where

{-

--- Day 5: Hydrothermal Venture ---

You come across a field of hydrothermal vents on the ocean floor! These vents constantly produce large, opaque clouds, so it would be best to avoid them if possible.

They tend to form in lines; the submarine helpfully produces a list of nearby lines of vents (your puzzle input) for you to review. For example:

0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2

Each line of vents is given as a line segment in the format x1,y1 -> x2,y2 where x1,y1 are the coordinates of one end the line segment and x2,y2 are the coordinates of the other end. These line segments include the points at both ends. In other words:

An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.
For now, only consider horizontal and vertical lines: lines where either x1 = x2 or y1 = y2.

So, the horizontal and vertical lines from the above list would produce the following diagram:

.......1..
..1....1..
..1....1..
.......1..
.112111211
..........
..........
..........
..........
222111....

In this diagram, the top left corner is 0,0 and the bottom right corner is 9,9. Each position is shown as the number of lines which cover that point or . if no line covers that point. The top-left pair of 1s, for example, comes from 2,2 -> 2,1; the very bottom row is formed by the overlapping lines 0,9 -> 5,9 and 0,9 -> 2,9.

To avoid the most dangerous areas, you need to determine the number of points where at least two lines overlap. In the above example, this is anywhere in the diagram with a 2 or larger - a total of 5 points.

Consider only horizontal and vertical lines. At how many points do at least two lines overlap?

--- Part Two ---

Unfortunately, considering only horizontal and vertical lines doesn't give you the full picture; you need to also consider diagonal lines.

Because of the limits of the hydrothermal vent mapping system, the lines in your list will only ever be horizontal, vertical, or a diagonal line at exactly 45 degrees. In other words:

An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.
Considering all lines from the above example would now produce the following diagram:

1.1....11.
.111...2..
..2.1.111.
...1.2.2..
.112313211
...1.2....
..1...1...
.1.....1..
1.......1.
222111....
You still need to determine the number of points where at least two lines overlap. In the above example, this is still anywhere in the diagram with a 2 or larger - now a total of 12 points.

Consider all of the lines. At how many points do at least two lines overlap?

-}

import Text.RawString.QQ (r)
import Text.Read (readMaybe) 
import Data.List (transpose, nub)
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (mapMaybe)
import Data.Bool (bool)
import Control.Arrow ( Arrow((&&&), (***)) )
import Data.Map.Strict as M (insertWith, fromListWith, elems)

testInput :: String
testInput = [r|
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
|]

-- Part 2

-- | Testing day5b
-- >>> day5b testInput
-- 12

day5b :: String -> Int
day5b = length . filter (>= 2) . counts'' . parseLines

drawLine :: Line -> [Point]
drawLine (p1@(x1,y1),p2@(x2,y2))
    | p1 == p2  = [p2]
    | otherwise = p1 : drawLine ((mt x2 *** mt y2) p1, p2)
    where
    mt b a | a < b = succ a
           | a > b = pred a
           | otherwise = a

counts'' :: [Line] -> [Int]
counts'' ls = filter (>=2) $ M.elems $ M.fromListWith (+) (map (,1) $ concatMap drawLine ls)

-- >>> (\ls -> putStrLn $ unlines $ map concat $ transpose $ chunksOf (length ls) $ map show $ counts' ls) (parseLines testInput)

-- Part 1

day5 :: String -> Int
day5  = length . filter (>= 2) . counts . parseLines

-- | Testing day5
-- >>> day5 testInput
-- 5

-- >>> (\ls -> putStrLn $ unlines $ map concat $ transpose $ chunksOf (length ls) $ map show $ counts ls) (parseLines testInput)

type Point =  (Integer, Integer)
type Line = (Point, Point) 

parseLines :: String -> [Line]
parseLines  = mapMaybe (parseLine . mapMaybe (pair . splitOn ",") . splitOn " -> ") . lines

pair :: [String] -> Maybe Point
pair [a,b] = do
    a' <- readMaybe a
    b' <- readMaybe b
    pure (a',b')
pair _ = Nothing

parseLine :: [Point] -> Maybe Line
parseLine [(a,b),(c,d)] = Just ((a,b),(c,d))
parseLine _ = Nothing

onLine :: Point -> Line -> Int
onLine (x,y) ((lx1,ly1),(lx2,ly2)) | x == lx1 && x == lx2 = bool 0 1 $ y >= min ly1 ly2 && y <= max ly1 ly2
onLine (x,y) ((lx1,ly1),(lx2,ly2)) | y == ly1 && y == ly2 = bool 0 1 $ x >= min lx1 lx2 && x <= max lx1 lx2
onLine _ _ = 0

-- | Testing onLine
-- >>> onLine (2,7) ((2,1),(2,3))
-- 0
-- >>> onLine (2,2) ((2,1),(2,3))
-- 1

counts :: [Line] -> [Int]
counts ls = M.elems $ M.fromListWith (+) (concatMap (map (,1) . drawLine) $ filter manhattan ls)

manhattan :: (Eq a1, Eq a2) => ((a1, a2), (a1, a2)) -> Bool
manhattan ((a,b),(c,d)) = a == c || b == d

-- | Testing counts
-- >>> counts [((0,0),(0,1)), ((0,0),(1,0))]
-- [2,1,1]

-- | Example input for day5
-- >>> parseLines testInput
-- [((0,9),(5,9)),((8,0),(0,8)),((9,4),(3,4)),((2,2),(2,1)),((7,0),(7,4)),((6,4),(2,0)),((0,9),(2,9)),((3,4),(1,4)),((0,0),(8,8)),((5,5),(8,2))]
