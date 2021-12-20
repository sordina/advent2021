{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NegativeLiterals #-}

module Advent12 where

{-

--- Day 12: Passage Pathing ---

With your submarine's subterranean subsystems subsisting suboptimally, the only way you're getting out of this cave anytime soon is by finding a path yourself. Not just a path - the only way to know if you've found the best path is to find all of them.

Fortunately, the sensors are still mostly working, and so you build a rough map of the remaining caves (your puzzle input). For example:

start-A
start-b
A-c
A-b
b-d
A-end
b-end

This is a list of how all of the caves are connected. You start in the cave named start, and your destination is the cave named end. An entry like b-d means that cave b is connected to cave d - that is, you can move between them.

So, the above cave system looks roughly like this:

    start
    /   \
c--A-----b--d
    \   /
     end

Your goal is to find the number of distinct paths that start at start, end at end, and don't visit small caves more than once. There are two types of caves: big caves (written in uppercase, like A) and small caves (written in lowercase, like b). It would be a waste of time to visit any small cave more than once, but big caves are large enough that it might be worth visiting them multiple times. So, all paths you find should visit small caves at most once, and can visit big caves any number of times.

Given these rules, there are 10 paths through this example cave system:

start,A,b,A,c,A,end
start,A,b,A,end
start,A,b,end
start,A,c,A,b,A,end
start,A,c,A,b,end
start,A,c,A,end
start,A,end
start,b,A,c,A,end
start,b,A,end
start,b,end

(Each line in the above list corresponds to a single path; the caves visited by that path are listed in the order they are visited and separated by commas.)

Note that in this cave system, cave d is never visited by any path: to do so, cave b would need to be visited twice (once on the way to cave d and a second time when returning from cave d), and since cave b is small, this is not allowed.

Here is a slightly larger example:

dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc

The 19 paths through it are as follows:

start,HN,dc,HN,end
start,HN,dc,HN,kj,HN,end
start,HN,dc,end
start,HN,dc,kj,HN,end
start,HN,end
start,HN,kj,HN,dc,HN,end
start,HN,kj,HN,dc,end
start,HN,kj,HN,end
start,HN,kj,dc,HN,end
start,HN,kj,dc,end
start,dc,HN,end
start,dc,HN,kj,HN,end
start,dc,end
start,dc,kj,HN,end
start,kj,HN,dc,HN,end
start,kj,HN,dc,end
start,kj,HN,end
start,kj,dc,HN,end
start,kj,dc,end

Finally, this even larger example has 226 paths through it:

fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW

How many paths through this cave system are there that visit small caves at most once?

--- Part Two ---

After reviewing the available paths, you realize you might have time to visit a single small cave twice. Specifically, big caves can be visited any number of times, a single small cave can be visited at most twice, and the remaining small caves can be visited at most once. However, the caves named start and end can only be visited exactly once each: once you leave the start cave, you may not return to it, and once you reach the end cave, the path must end immediately.

Now, the 36 possible paths through the first example above are:

start,A,b,A,b,A,c,A,end
start,A,b,A,b,A,end
...

The slightly larger example above now has 103 paths through it, and the even larger example now has 3509 paths through it.

Given these new rules, how many paths through this cave system are there?

-}

import Text.RawString.QQ (r)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Char (isLower)
import Data.List (nub)

-- | Testing day12
-- >>> day12 testInput
-- 10

-- | Testing day12 medium
-- >>> day12 testInputMedium
-- 19

-- | Testing day12 large
-- >>> day12 testInputLarge
-- 226


-- | Testing day12b
-- >>> day12b testInput
-- 36

-- | Testing day12b medium
-- >>> day12b testInputMedium
-- 103

-- | Testing day12b large
-- >>> day12b testInputLarge
-- 3509


day12 :: String -> Int
day12 = length . paths "start" Set.empty . parseInput

paths :: String -> Set.Set String -> Map.Map String (Set.Set String) -> [[String]]
paths "end" _ _ = [["end"]]
paths n v m
  | n `Set.member` v = []
  | otherwise = do
    let xs = maybe [] Set.toList (Map.lookup n m)
    x <- xs
    (n:) <$> if isLower (head n)
      then paths x (Set.insert n v) m
      else paths x v m

-- | Testing paths
-- >>> paths "start" Set.empty $ parseInput testInput
-- [["start","A","b","A","c","A","end"],["start","A","b","A","end"],["start","A","b","end"],["start","A","c","A","b","A","end"],["start","A","c","A","b","end"],["start","A","c","A","end"],["start","A","end"],["start","b","A","c","A","end"],["start","b","A","end"],["start","b","end"]]

day12b :: String -> Int
day12b = length . paths2' "start" Map.empty . Map.map (Set.delete "start") . parseInput

paths2' :: String -> Map.Map String Int -> Map.Map String (Set.Set String) -> Set.Set [String]
paths2' n v m = Set.unions $ map (Set.fromList . paths2 n v m) rs
  where
  rs =  Map.keys m

paths2 :: String -> Map.Map String Int -> Map.Map String (Set.Set String) -> String -> [[String]]
paths2 "end" _ _ _ = [["end"]]
paths2 n v m r
  |           c >= 2 = []
  | n /= r && c == 1 = []
  | otherwise = do
    let xs = maybe [] Set.toList (Map.lookup n m)
    x <- xs
    (n:) <$> if isLower (head n)
      then paths2 x (Map.insertWith (+) n 1 v) m r
      else paths2 x v m r
  where
  c = fromMaybe 0 (Map.lookup n v)

-- Helpers

parseInput :: String -> Map.Map String (Set.Set String)
parseInput = Map.fromListWith Set.union . concatMap (pair . splitOn "-") . words

pair :: [b] -> [(b, Set.Set b)]
pair [a,b] = [(a, Set.singleton b), (b, Set.singleton a)]
pair _     = []

testInput :: String
testInput = unlines $ words [r|
start-A
start-b
A-c
A-b
b-d
A-end
b-end
|]

testInputMedium :: String
testInputMedium = unlines $ words [r|
dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
|]

testInputLarge :: String
testInputLarge = unlines $ words [r|
fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
|]

{- graphviz:

  graph {
    start -- A
    start -- b
    A -- c
    A -- b
    b -- d
    A -- end
    b -- end
  }
-}
