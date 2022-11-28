{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | Run with `cat data/day22.input.small | cabal run advent2021 22b`
module Advent22b where

import Advent22 qualified as A22
import Advent22BCuboids qualified as A22BCuboids
import Data.Set qualified as Set
import Text.RawString.QQ (r)
import Data.Foldable (foldl')

day22b :: String -> Int
day22b = sum . map A22BCuboids.volume . Set.toList . process . map cubify . A22.parseInput

process :: [(Bool, A22BCuboids.Cuboid)] -> Set.Set A22BCuboids.Cuboid
process = foldl' apply Set.empty

-- | Testing a very simple 2d inputs
-- >>> process [(True, A22BCuboids.Cuboid [(0,3),(0,3)])]
-- >>> process [(True, A22BCuboids.Cuboid [(0,3),(0,3)]), (False, A22BCuboids.Cuboid [(3,4),(3,4)])]
-- >>> process [(True, A22BCuboids.Cuboid [(0,3),(0,3)]), (True, A22BCuboids.Cuboid [(3,6),(3,6)])]
-- >>> process [(True, A22BCuboids.Cuboid [(0,3),(0,3)]), (True, A22BCuboids.Cuboid [(3,6),(3,6)]), (False, A22BCuboids.Cuboid [(3,3),(3,3)])]
-- fromList [Cuboid [(0,3),(0,3)]]
-- fromList [Cuboid [(0,2),(0,2)],Cuboid [(0,2),(3,3)],Cuboid [(3,3),(0,2)]]
-- fromList [Cuboid [(0,2),(0,2)],Cuboid [(0,2),(3,3)],Cuboid [(3,3),(0,2)],Cuboid [(3,6),(3,6)]]
-- fromList [Cuboid [(0,2),(0,2)],Cuboid [(0,2),(3,3)],Cuboid [(3,3),(0,2)]]

apply :: Set.Set A22BCuboids.Cuboid -> (Bool, A22BCuboids.Cuboid) -> Set.Set A22BCuboids.Cuboid
apply s (False, c) = Set.unions $ Set.map (A22BCuboids.-~ c) s -- EZ
apply s (True,  c) = Set.insert c $ Set.unions $ Set.map (A22BCuboids.-~ c) s -- Why not?? :D

cubify :: (a, (A22BCuboids.P2, A22BCuboids.P2, A22BCuboids.P2)) -> (a, A22BCuboids.Cuboid)
cubify (b, (x,y,z)) = (b, A22BCuboids.Cuboid [x,y,z])

-- | Testing day22 on testInput
-- >>> day22b testInputSmall
-- 19

-- | Testing parseInput on testInput
-- >>> A22.parseInput testInputSmall
-- [(True,((10,12),(10,12),(10,12))),(True,((11,13),(11,13),(11,13))),(False,((9,11),(9,11),(9,11))),(True,((10,10),(10,10),(10,10)))]

testInputSmall :: String
testInputSmall = unlines $ tail $ lines [r|
on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10
|]
