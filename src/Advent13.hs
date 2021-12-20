{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NegativeLiterals #-}

module Advent13 where

{-

--- Day 13: Transparent Origami ---

You reach another volcanically active part of the cave. It would be nice if you could do some kind of thermal imaging so you could tell ahead of time which caves are too hot to safely enter.

Fortunately, the submarine seems to be equipped with a thermal camera! When you activate it, you are greeted with:

Congratulations on your purchase! To activate this infrared thermal imaging
camera system, please enter the code found on page 1 of the manual.
Apparently, the Elves have never used this feature. To your surprise, you manage to find the manual; as you go to open it, page 1 falls out. It's a large sheet of transparent paper! The transparent paper is marked with random dots and includes instructions on how to fold it up (your puzzle input). For example:

6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
The first section is a list of dots on the transparent paper. 0,0 represents the top-left coordinate. The first value, x, increases to the right. The second value, y, increases downward. So, the coordinate 3,0 is to the right of 0,0, and the coordinate 0,7 is below 0,0. The coordinates in this example form the following pattern, where # is a dot on the paper and . is an empty, unmarked position:

...#..#..#.
....#......
...........
#..........
...#....#.#
...........
...........
...........
...........
...........
.#....#.##.
....#......
......#...#
#..........
#.#........
Then, there is a list of fold instructions. Each instruction indicates a line on the transparent paper and wants you to fold the paper up (for horizontal y=... lines) or left (for vertical x=... lines). In this example, the first fold instruction is fold along y=7, which designates the line formed by all of the positions where y is 7 (marked here with -):

...#..#..#.
....#......
...........
#..........
...#....#.#
...........
...........
-----------
...........
...........
.#....#.##.
....#......
......#...#
#..........
#.#........
Because this is a horizontal line, fold the bottom half up. Some of the dots might end up overlapping after the fold is complete, but dots will never appear exactly on a fold line. The result of doing this fold looks like this:

#.##..#..#.
#...#......
......#...#
#...#......
.#.#..#.###
...........
...........
Now, only 17 dots are visible.

Notice, for example, the two dots in the bottom left corner before the transparent paper is folded; after the fold is complete, those dots appear in the top left corner (at 0,0 and 0,1). Because the paper is transparent, the dot just below them in the result (at 0,3) remains visible, as it can be seen through the transparent paper.

Also notice that some dots can end up overlapping; in this case, the dots merge together and become a single dot.

The second fold instruction is fold along x=5, which indicates this line:

#.##.|#..#.
#...#|.....
.....|#...#
#...#|.....
.#.#.|#.###
.....|.....
.....|.....
Because this is a vertical line, fold left:

#####
#...#
#...#
#...#
#####
.....
.....
The instructions made a square!

The transparent paper is pretty big, so for now, focus on just completing the first fold. After the first fold in the example above, 17 dots are visible - dots that end up overlapping after the fold is completed count as a single dot.

How many dots are visible after completing just the first fold instruction on your transparent paper?


--- Part Two ---

Finish folding the transparent paper according to the instructions. The manual says the code is always eight capital letters.

What code do you use to activate the infrared thermal imaging camera system?

-}

import Text.RawString.QQ (r)
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.Foldable (foldl')
import Data.Bool (bool)
import Control.Arrow ((&&&))

-- | Testing day12
-- >>> day13 testInput
-- 17

day13 :: String -> Int
day13 = Set.size . (\(ps,fs) -> foldl' fold ps [head fs]) . parseInput

day13b :: String -> String
day13b = display . uncurry (foldl' fold) . parseInput

display :: Set.Set (Int,Int) -> String
display s = unlines ls
  where
    ps = Set.toList s
    ls = map mkL [miny..maxy]
    (miny, maxy) = (minimum &&& maximum) $ map snd ps
    (minx, maxx) = (minimum &&& maximum) $ map fst ps
    mkL y = map (bool ' ' '#' . flip Set.member s . (,y)) [minx..maxx]


-- >>> (\(ps,fs) -> foldl' fold ps [head fs]) $ parseInput testInput
-- fromList [(0,0),(0,1),(0,11),(1,4),(2,0),(3,10),(3,14),(4,3),(4,13),(6,2),(6,4),(6,14),(8,4),(8,10),(9,4),(9,14),(10,2),(10,10)]

fold :: Set.Set (Int,Int) -> (Char,Int) -> Set.Set (Int,Int)
fold s ('x',p) = Set.map (\c@(x,y) -> bool c (p-(x-p),y) (x>p)) s
fold s ('y',p) = Set.map (\c@(x,y) -> bool c (x,p-(y-p)) (y>p)) s
fold _ (c,  _) = error $ "oops: " <> show c

-- Helpers

parseInput :: String -> (Set.Set (Int,Int),[(Char,Int)])
parseInput i = (dsS, fsL)
  where
    ls = lines i
    (ds:fs:_) = splitOn [""] ls
    dsS = Set.fromList $ map ((\[a,b] -> (a,b)) . map read . splitOn ",") ds
    fsL = map ((\[a,p] -> (head a, read p)) . splitOn "=" . last . words) fs

-- >>> parseInput testInput
-- (fromList [(0,3),(0,13),(0,14),(1,10),(2,14),(3,0),(3,4),(4,1),(4,11),(6,0),(6,10),(6,12),(8,4),(8,10),(9,0),(9,10),(10,4),(10,12)],[('y',7),('x',5)])

testInput :: String
testInput = unlines $ drop 1 $ lines [r|
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
|]
