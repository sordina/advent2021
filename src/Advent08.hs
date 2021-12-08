{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Advent08 where

{-

--- Day 8: Seven Segment Search ---

You barely reach the safety of the cave when the whale smashes into the cave mouth, collapsing it. Sensors indicate another exit to this cave at a much greater depth, so you have no choice but to press on.

As your submarine slowly makes its way through the cave system, you notice that the four-digit seven-segment displays in your submarine are malfunctioning; they must have been damaged during the escape. You'll be in a lot of trouble without them, so you'd better figure out what's wrong.

Each digit of a seven-segment display is rendered by turning on or off any of seven segments named a through g:

  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg
So, to render a 1, only segments c and f would be turned on; the rest would be off. To render a 7, only segments a, c, and f would be turned on.

The problem is that the signals which control the segments have been mixed up on each display. The submarine is still trying to display numbers by producing output on signal wires a through g, but those wires are connected to segments randomly. Worse, the wire/segment connections are mixed up separately for each four-digit display! (All of the digits within a display use the same connections, though.)

So, you might know that only signal wires b and g are turned on, but that doesn't mean segments b and g are turned on: the only digit that uses two segments is 1, so it must mean segments c and f are meant to be on. With just that information, you still can't tell which wire (b/g) goes to which segment (c/f). For that, you'll need to collect more information.

For each display, you watch the changing signals for a while, make a note of all ten unique signal patterns you see, and then write down a single four digit output value (your puzzle input). Using the signal patterns, you should be able to work out which pattern corresponds to which digit.

For example, here is what you might see in a single entry in your notes:

acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
cdfeb fcadb cdfeb cdbaf
(The entry is wrapped here to two lines so it fits; in your notes, it will all be on a single line.)

Each entry consists of ten unique signal patterns, a | delimiter, and finally the four digit output value. Within an entry, the same wire/segment connections are used (but you don't know what the connections actually are). The unique signal patterns correspond to the ten different ways the submarine tries to render a digit using the current wire/segment connections. Because 7 is the only digit that uses three segments, dab in the above example means that to render a 7, signal lines d, a, and b are on. Because 4 is the only digit that uses four segments, eafb means that to render a 4, signal lines e, a, f, and b are on.

Using this information, you should be able to work out which combination of signal wires corresponds to each of the ten digits. Then, you can decode the four digit output value. Unfortunately, in the above example, all of the digits in the output value (cdfeb fcadb cdfeb cdbaf) use five segments and are more difficult to deduce.

For now, focus on the easy digits. Consider this larger example:

be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |
fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |
cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |
efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |
gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |
gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |
cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |
ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |
gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |
fgae cfgab fg bagce
Because the digits 1, 4, 7, and 8 each use a unique number of segments, you should be able to tell which combinations of signals correspond to those digits. Counting only digits in the output values (the part after | on each line), in the above example, there are 26 instances of digits that use a unique number of segments (highlighted above).

In the output values, how many times do digits 1, 4, 7, or 8 appear?

--- Part Two ---

Through a little deduction, you should now be able to determine the remaining digits. Consider again the first example above:

acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
cdfeb fcadb cdfeb cdbaf
After some careful analysis, the mapping between signal wires and segments only make sense in the following configuration:

 dddd
e    a
e    a
 ffff
g    b
g    b
 cccc
So, the unique signal patterns would correspond to the following digits:

acedgfb: 8
cdfbe: 5
gcdfa: 2
fbcad: 3
dab: 7
cefabd: 9
cdfgeb: 6
eafb: 4
cagedb: 0
ab: 1
Then, the four digits of the output value can be decoded:

cdfeb: 5
fcadb: 3
cdfeb: 5
cdbaf: 3
Therefore, the output value for this entry is 5353.

Following this same process for each entry in the second, larger example above, the output value of each entry can be determined:

fdgacbe cefdb cefbgd gcbe: 8394
fcgedb cgb dgebacf gc: 9781
cg cg fdcagb cbg: 1197
efabcd cedba gadfec cb: 9361
gecf egdcabf bgf bfgea: 4873
gebdcfa ecba ca fadegcb: 8418
cefg dcbef fcge gbcadfe: 4548
ed bcgafe cdgba cbgef: 1625
gbdfcae bgc cg cgb: 8717
fgae cfgab fg bagce: 4315
Adding all of the output values in this larger example produces 61229.

For each entry, determine all of the wire/segment connections and decode the four-digit output values. What do you get if you add up all of the output values?

-}

import Data.List.Split (splitOn)
import Text.RawString.QQ (r)
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)
import Data.List (sort, group, sortBy, groupBy, findIndex)
import Control.Arrow ((&&&), Arrow (second, (***)))
import Data.Ord (comparing)
import Data.Function (on)
import Data.Map (Map, unionsWith)
import Data.Set (intersection, Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Tuple (swap)
import Debug.Trace (traceShow)

-- | Testing day8
-- >>> day8 testInput 
-- 26

day8 :: String -> Int
day8 = sum . map solve . parseInput

parseInput :: String -> [([String],[String])]
parseInput = mapMaybe (inout . splitOn ["|"] . map sort . words) . lines

inout :: [b] -> Maybe (b, b)
inout [a,b] = Just (a,b)
inout _     = Nothing

-- | Testing parseInput
-- >>> parseInput $ head $ filter (not . null) $ lines testInput
-- [(["be","abcdefg","bcdefg","acdefg","bceg","cdefg","abdefg","bcdef","abcdf","bde"],["abcdefg","bcdef","bcdefg","bceg"])]

solve :: ([String], [String]) -> Int
solve (input, output) = length $ filter (`elem` special) output
  where
  special = concatMap fst $ filter ((==1) . snd) (counts input)

counts :: [String] -> [([String],Int)]
counts = map (map fst &&& length) . groupBy ((==) `on` snd) . sortBy (comparing snd) . map (id &&& length)

-- | Testing counts
-- >>> counts (words "a bb ccc dddd e ff g")
-- [(["a","e","g"],3),(["bb","ff"],2),(["ccc"],1),(["dddd"],1)]

-- Part 2

day8b :: String -> Integer
day8b = sum . map solve2 . parseInput2

parseInput2 :: String -> [(Set (Set Char), [Set Char])]
parseInput2 = map ( Set.fromList . map Set.fromList *** map Set.fromList) . parseInput

-- | Testing day8b
-- >>> day8b testInput
-- 61229

solve2 :: (Set (Set Char), [Set Char]) -> Integer
solve2 = read . digits

digits :: (Set (Set Char), [Set Char]) -> String
digits (input, output) = mapMaybe (`lookup` segmentMapInv) segmentsMatch
  where
  segmentsMatch = map (Set.fromList . mapMaybe corresponds . Set.toList) output
  corresponds c = Map.lookup c finalMap
  finalMap      = reMap $ filter (testMappings input) permutedMap
  permutedMap   = permuteMap subMapping
  subMapping    = fixEq (step input) openMapping
  openMapping   = Map.fromList $ map (, lettersSet) letters
  lettersSet    = Set.fromList letters
  letters       = ['a'..'g']

-- | Testing digits
-- >>> map digits $ parseInput2 $ head $ filter (not . null) $ lines testInput
-- ["8394"]

fixEq :: Eq a => (a -> a) -> a -> a
fixEq f x
  | x == y    = x
  | otherwise = fixEq f y
  where y = f x

step :: Set (Set Char) -> Map Char (Set Char) -> Map Char (Set Char)
step ss m = unionsWith intersection $ map (matchingLengths ss m) (Set.toList ss)

matchingLengths :: Set (Set Char) -> Map Char (Set Char) -> Set Char -> Map Char (Set Char)
matchingLengths ss m s = Map.fromList $ map (,bar) (Set.toList s)
  where
  bar = Set.unions $ Set.filter ((== Set.size s) . Set.size) segments

reMap :: [[(Char,Char)]] -> Map Char Char
reMap = Map.unions . map Map.fromList

permuteMap :: Map Char (Set Char) -> [[(Char, Char)]]
permuteMap = permute . Map.toList

permute :: [(Char, Set Char)] -> [[(Char,Char)]]
permute [] = [[]]
permute ((c,s):xs) = do
  r <- Set.toList s
  map ((c,r):) $ permute (expunge r xs)

-- | Testing permute
-- >>> permute [('x', (Set.fromList "abc")), ('y', (Set.fromList "fab"))]
-- [[('x','a'),('y','b')],[('x','a'),('y','f')],[('x','b'),('y','a')],[('x','b'),('y','f')],[('x','c'),('y','a')],[('x','c'),('y','b')],[('x','c'),('y','f')]]

expunge :: Char -> [(Char, Set Char)] -> [(Char, Set Char)]
expunge c = map (second (Set.delete c))

-- | Testing expunge
-- >>> expunge 'a' [('x', (Set.fromList "abc")), ('y', (Set.fromList "fab"))]
-- [('x',fromList "bc"),('y',fromList "bf")]

testMappings :: Set (Set Char) -> [(Char,Char)] -> Bool
testMappings ws m = segments == Set.fromList (map (getMapping m . Set.toList) (Set.toList ws))

getMapping :: [(Char,Char)] -> String -> Set Char
getMapping m = Set.fromList . mapMaybe (`lookup` m)

-- Data

testInput :: String
testInput = [r|
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
|]

segmentMapInv :: [(Set Char, Char)]
segmentMapInv = map swap segmentMap

segmentMap :: [(Char, Set Char)]
segmentMap = map (second Set.fromList) [('0', "abcefg"),('1',"cf"),('2',"acdeg"),('3',"acdfg"),('4',"bcdf"),('5',"abdfg"),('6',"abdefg"),('7',"acf"),('8',"abcdefg"),('9',"abcdfg")]

segments :: Set (Set Char)
segments = Set.fromList $ map snd segmentMap

{- Table of Digits Canonical Mapping

  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg
-}
