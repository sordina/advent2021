{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Advent22 where

{-

--- Day 22: Reactor Reboot ---

Operating at these extreme ocean depths has overloaded the submarine's reactor; it needs to be rebooted.

The reactor core is made up of a large 3-dimensional grid made up entirely of cubes, one cube per integer 3-dimensional coordinate (x,y,z). Each cube can be either on or off; at the start of the reboot process, they are all off. (Could it be an old model of a reactor you've seen before?)

To reboot the reactor, you just need to set all of the cubes to either on or off by following a list of reboot steps (your puzzle input). Each step specifies a cuboid (the set of all cubes that have coordinates which fall within ranges for x, y, and z) and whether to turn all of the cubes in that cuboid on or off.

For example, given these reboot steps:

on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10

The first step (on x=10..12,y=10..12,z=10..12) turns on a 3x3x3 cuboid consisting of 27 cubes:

10,10,10
10,10,11
10,10,12
10,11,10
10,11,11
10,11,12
10,12,10
10,12,11
10,12,12
11,10,10
11,10,11
11,10,12
11,11,10
11,11,11
11,11,12
11,12,10
11,12,11
11,12,12
12,10,10
12,10,11
12,10,12
12,11,10
12,11,11
12,11,12
12,12,10
12,12,11
12,12,12

The second step (on x=11..13,y=11..13,z=11..13) turns on a 3x3x3 cuboid that overlaps with the first. As a result, only 19 additional cubes turn on; the rest are already on from the previous step:

11,11,13
11,12,13
11,13,11
11,13,12
11,13,13
12,11,13
12,12,13
12,13,11
12,13,12
12,13,13
13,11,11
13,11,12
13,11,13
13,12,11
13,12,12
13,12,13
13,13,11
13,13,12
13,13,13

The third step (off x=9..11,y=9..11,z=9..11) turns off a 3x3x3 cuboid that overlaps partially with some cubes that are on, ultimately turning off 8 cubes:

10,10,10
10,10,11
10,11,10
10,11,11
11,10,10
11,10,11
11,11,10
11,11,11

The final step (on x=10..10,y=10..10,z=10..10) turns on a single cube, 10,10,10. After this last step, 39 cubes are on.

The initialization procedure only uses cubes that have x, y, and z positions of at least -50 and at most 50. For now, ignore cubes outside this region.

Here is a larger example:

on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682

The last two steps are fully outside the initialization procedure area; all other steps are fully within it. After executing these steps in the initialization procedure region, 590784 cubes are on.

Execute the reboot steps. Afterward, considering only cubes in the region x=-50..50,y=-50..50,z=-50..50, how many cubes are on?

-}

import Text.RawString.QQ (r)
import Data.List.Split ( splitOn )
import Control.Arrow ((&&&))
import Data.Monoid (Last (Last, getLast))
import Data.Maybe (mapMaybe, fromMaybe)

-- | Testing day22
-- >>> day22 testInput
-- 39
-- 
-- >>> day22 testInput2
-- 590784

-- | Testing parseInput
-- >>> parseInput testInput
-- [(True,((10,12),(10,12),(10,12))),(True,((11,13),(11,13),(11,13))),(False,((9,11),(9,11),(9,11))),(True,((10,10),(10,10),(10,10)))]
-- 
-- >>> parseInput testInput2
-- [(True,((-20,26),(-36,17),(-47,7))),(True,((-20,33),(-21,23),(-26,28))),(True,((-22,28),(-29,23),(-38,16))),(True,((-46,7),(-6,46),(-50,-1))),(True,((-49,1),(-3,46),(-24,28))),(True,((2,47),(-22,22),(-23,27))),(True,((-27,23),(-28,26),(-21,29))),(True,((-39,5),(-6,47),(-3,44))),(True,((-30,21),(-8,43),(-13,34))),(True,((-22,26),(-27,20),(-29,19))),(False,((-48,-32),(26,41),(-47,-37))),(True,((-12,35),(6,50),(-50,-2))),(False,((-48,-32),(-32,-16),(-15,-5))),(True,((-18,26),(-33,15),(-7,46))),(False,((-40,-22),(-38,-28),(23,41))),(True,((-16,35),(-41,10),(-47,6))),(False,((-32,-23),(11,30),(-14,3))),(True,((-49,-5),(-3,45),(-29,18))),(False,((18,30),(-20,-8),(-3,13))),(True,((-41,9),(-7,43),(-33,15))),(True,((-54112,-39298),(-85059,-49293),(-27449,7877))),(True,((967,23432),(45373,81175),(27513,53682)))]

testInput :: String
testInput = unlines $ tail $ lines [r|
on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10
|]

testInput2 :: String
testInput2 = unlines $ tail $ lines [r|
on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682
|]

test :: (Int,Int,Int) -> (Bool, ((Int,Int),(Int,Int),(Int,Int))) -> Maybe Bool
test (x,y,z) (b,(x',y',z')) | x <? x' && y <? y' && z <? z' = Just b
test _ _ = Nothing

tests :: (Int,Int,Int) -> [(Bool, ((Int,Int),(Int,Int),(Int,Int)))] -> Bool
tests p = fromMaybe False . getLast . mconcat . map (Last . test p)

(<?) :: Int -> (Int,Int) -> Bool
p <? (l,h) = p >= l && p <= h

day22 :: String -> Int
day22 i = length $ filter id $ map (`tests` parsed) cs
    where
    r = [-50..50]
    cs = [(x,y,z) | x <- r, y <- r, z <- r]
    parsed = parseInput i

parseInput :: String -> [(Bool, ((Int,Int),(Int,Int),(Int,Int)))]
parseInput = map parseLine . filter (not . null) . lines
    where
    parseLine = (flag . head &&& coords . last) . words

    flag "on" = True
    flag "off" = False
    flag _ = error "Partial flag"

    coords = coord . map parseWord . splitOn ","

    coord [('x',(xl,xh)), ('y', (yl,yh)), ('z', (zl,zh))] = ((xl,xh), (yl,yh), (zl,zh))
    coord _ = error "partial coord"

    parseWord (a:'=':ns) = (a, parseNs ns)
    parseWord _ = error "partial word"

    parseNs ns = case splitOn ".." ns of [l,h] -> (read l, read h)
