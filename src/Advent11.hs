{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Advent11 where

{-

--- Day 11: Dumbo Octopus ---

You enter a large cavern full of rare bioluminescent dumbo octopuses! They seem to not like the Christmas lights on your submarine, so you turn them off for now.

There are 100 octopuses arranged neatly in a 10 by 10 grid. Each octopus slowly gains energy over time and flashes brightly for a moment when its energy is full. Although your lights are off, maybe you could navigate through the cave without disturbing the octopuses if you could predict when the flashes of light will happen.

Each octopus has an energy level - your submarine can remotely measure the energy level of each octopus (your puzzle input). For example:

5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526

The energy level of each octopus is a value between 0 and 9. Here, the top-left octopus has an energy level of 5, the bottom-right one has an energy level of 6, and so on.

You can model the energy levels and flashes of light in steps. During a single step, the following occurs:

First, the energy level of each octopus increases by 1.
Then, any octopus with an energy level greater than 9 flashes. This increases the energy level of all adjacent octopuses by 1, including octopuses that are diagonally adjacent. If this causes an octopus to have an energy level greater than 9, it also flashes. This process continues as long as new octopuses keep having their energy level increased beyond 9. (An octopus can only flash at most once per step.)
Finally, any octopus that flashed during this step has its energy level set to 0, as it used all of its energy to flash.
Adjacent flashes can cause an octopus to flash on a step even if it begins that step with very little energy. Consider the middle octopus with 1 energy in this situation:

Before any steps:

11111
19991
19191
19991
11111

After step 1:

34543
40004
50005
40004
34543

After step 2:

45654
51115
61116
51115
45654

An octopus is highlighted when it flashed during the given step.

Here is how the larger example above progresses:

Before any steps:

5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526

After step 1:

6594254334
3856965822
6375667284
7252447257
7468496589
5278635756
3287952832
7993992245
5957959665
6394862637

After step 2:

8807476555
5089087054
8597889608
8485769600
8700908800
6600088989
6800005943
0000007456
9000000876
8700006848

...

After step 100:

0397666866
0749766918
0053976933
0004297822
0004229892
0053222877
0532222966
9322228966
7922286866
6789998766

After 100 steps, there have been a total of 1656 flashes.

Given the starting energy levels of the dumbo octopuses in your cavern, simulate 100 steps. How many total flashes are there after 100 steps?

-}

import Text.RawString.QQ (r)
import Utils ( fixEq )
import Control.Arrow ((&&&))
import Data.List ((\\))
import Control.Lens ( (%=), At(at), (.=) )

import Data.Map            qualified as Map
import Control.Monad.State qualified as State

-- | Testing day11
-- >>> day11 testInput 
-- 1656

day11 :: String -> Int
day11 = sum . map (length . flashedG) . take 101 . generations . parseInput

data Generation = G Bounds World deriving Show

type Input    = (Bounds, World)
type Bounds   = (Int,Int)
type World    = Map.Map (Int,Int) Int
type Flashing = [(Int,Int)]

generations :: Input -> [Generation]
generations = iterate step . firstGeneration

firstGeneration :: Input -> Generation
firstGeneration = uncurry G

-- | Testing firstGeneration
-- >>> firstGeneration $ parseInput testInputSmall
-- G (2,2) (fromList [((0,0),5),((0,1),2),((1,0),4),((1,1),7)])

flashing :: World -> Flashing
flashing = map fst . filter ((>9) . snd) . Map.toList

flashedG :: Generation -> Flashing
flashedG (G b w) = flashed w

flashed :: World -> Flashing
flashed = map fst . filter ((==0) . snd) . Map.toList

step :: Generation -> Generation
step (G b w) = G b (brighten w)

brighten :: World -> World
brighten = fixEq subsequentBrightening . Map.map succ

subsequentBrightening :: World -> World
subsequentBrightening = State.execState do
  fs <- State.gets flashing
  mapM_ updateAdjacent fs
  mapM_ (\p -> at p .= Just 0) fs

-- | Testing subsequentBrightening
-- >>> subsequentBrightening (Map.fromList [((0,0),3),((1,1),9),((2,2),10)])
-- fromList [((0,0),3),((1,1),10),((2,2),0)]
-- >>> subsequentBrightening $ subsequentBrightening $ subsequentBrightening (Map.fromList [((0,0),3),((1,1),9),((2,2),10)])
-- fromList [((0,0),4),((1,1),0),((2,2),0)]


updateAdjacent :: (Int,Int) -> State.State World ()
updateAdjacent p@(x,y) = State.forM_ ps \p -> at p %= fmap inc
  where
  ps = [(x',y') | x' <- [pred x..succ x], y' <- [pred y..succ y]]
  inc 0 = 0
  inc n = succ n

-- | Testing brighten
-- 
-- >>> brighten $ Map.fromList [((0,0),7),((0,1),8),((1,0),9),((1,3),1)]
-- fromList [((0,0),0),((0,1),0),((1,0),0),((1,3),2)]

parseInput :: String -> Input
parseInput i = (bounds, world)
  where
  world  = Map.fromList coords
  coords = concatMap (\(y,l) -> zipWith (\x c -> ((x, y), read [c])) [0..] l) (zip [0..] ls)
  bounds = (width, height)
  height = length ls
  width  = length $ head ls
  ls     = lines i

-- | Testing parseInput
-- >>> parseInput testInputSmall
-- ((2,2),fromList [((0,0),5),((0,1),2),((1,0),4),((1,1),7)])

-- Data

testInputSmall :: String
testInputSmall = unlines $ map (take 2) (take 2 (lines testInput))

testInput :: String
testInput = unlines $ words [r|
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
|]
