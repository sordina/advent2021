{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Advent22b where

import Advent22 qualified as A22
import Advent22BCuboids qualified as A22BCuboids
import Data.Set qualified as Set
import Text.RawString.QQ (r)

day22b :: String -> Int
day22b = sum . map A22BCuboids.volume . Set.toList . foldl insert Set.empty . map instruct . A22.parseInput

insert :: Set.Set A22BCuboids.Cuboid -> (Bool, A22BCuboids.Cuboid) -> Set.Set A22BCuboids.Cuboid
insert s (True, c) = s
insert s (False, c) = s

instruct :: (a, (A22BCuboids.P2, A22BCuboids.P2, A22BCuboids.P2)) -> (a, A22BCuboids.Cuboid)
instruct (b, (x,y,z)) = (b, A22BCuboids.Cuboid [x,y,z])

-- | Testing day22 on testInput
-- >>> day22b testInput

-- | Testing parseInput on testInput
-- >>> A22.parseInput testInput
-- [(True,((10,12),(10,12),(10,12))),(True,((11,13),(11,13),(11,13))),(False,((9,11),(9,11),(9,11))),(True,((10,10),(10,10),(10,10)))]

testInput :: String
testInput = unlines $ tail $ lines [r|
on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10
|]
