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

day22b :: String -> Int
day22b = sum . map A22BCuboids.volume . Set.toList . foldl apply Set.empty . map cubify . A22.parseInput

apply :: Set.Set A22BCuboids.Cuboid -> (Bool, A22BCuboids.Cuboid) -> Set.Set A22BCuboids.Cuboid
apply s (False, c) = Set.unions $ Set.map (A22BCuboids.-~ c) s -- EZ
apply s (True,  c) = Set.insert c $ Set.unions $ Set.map (A22BCuboids.-~ c) s -- Why not?? :D

cubify :: (a, (A22BCuboids.P2, A22BCuboids.P2, A22BCuboids.P2)) -> (a, A22BCuboids.Cuboid)
cubify (b, (x,y,z)) = (b, A22BCuboids.Cuboid [x,y,z])

-- | Testing day22 on testInput
-- >>> day22b testInputSmall
-- 26

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
