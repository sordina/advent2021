{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NegativeLiterals #-}

module Advent14 where

{-

--- Day 14: Extended Polymerization ---

The incredible pressures at this depth are starting to put a strain on your submarine. The submarine has polymerization equipment that would produce suitable materials to reinforce the submarine, and the nearby volcanically-active caves should even have the necessary input elements in sufficient quantities.

The submarine manual contains instructions for finding the optimal polymer formula; specifically, it offers a polymer template and a list of pair insertion rules (your puzzle input). You just need to work out what polymer would result after repeating the pair insertion process a few times.

For example:

NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
The first line is the polymer template - this is the starting point of the process.

The following section defines the pair insertion rules. A rule like AB -> C means that when elements A and B are immediately adjacent, element C should be inserted between them. These insertions all happen simultaneously.

So, starting with the polymer template NNCB, the first step simultaneously considers all three pairs:

The first pair (NN) matches the rule NN -> C, so element C is inserted between the first N and the second N.
The second pair (NC) matches the rule NC -> B, so element B is inserted between the N and the C.
The third pair (CB) matches the rule CB -> H, so element H is inserted between the C and the B.
Note that these pairs overlap: the second element of one pair is the first element of the next pair. Also, because all pairs are considered simultaneously, inserted elements are not considered to be part of a pair until the next step.

After the first step of this process, the polymer becomes NCNBCHB.

Here are the results of a few steps using the above rules:

Template:     NNCB
After step 1: NCNBCHB
After step 2: NBCCNBBBCBHCB
After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB
This polymer grows quickly. After step 5, it has length 97; After step 10, it has length 3073. After step 10, B occurs 1749 times, C occurs 298 times, H occurs 161 times, and N occurs 865 times; taking the quantity of the most common element (B, 1749) and subtracting the quantity of the least common element (H, 161) produces 1749 - 161 = 1588.

Apply 10 steps of pair insertion to the polymer template and find the most and least common elements in the result. What do you get if you take the quantity of the most common element and subtract the quantity of the least common element?


--- Part Two ---

The resulting polymer isn't nearly strong enough to reinforce the submarine. You'll need to run more steps of the pair insertion process; a total of 40 steps should do it.

In the above example, the most common element is B (occurring 2192039569602 times) and the least common element is H (occurring 3849876073 times); subtracting these produces 2188189693529.

Apply 40 steps of pair insertion to the polymer template and find the most and least common elements in the result. What do you get if you take the quantity of the most common element and subtract the quantity of the least common element?

-}

import Text.RawString.QQ (r)
import Data.List.Split (splitOn)
import Control.Arrow ((&&&), Arrow (first))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (group, sort, minimumBy, sortOn)
import Data.Ord (comparing)

-- | Testing day14b
-- >>> day14b testInput
-- 2188189693529

-- | Testing solve2
-- >>> sortOn snd . Map.toList . postProcess . (!!40) . solve2 . first preProcess . parseInput $ testInput
-- [('H',3849876073),('C',6597635301),('N',1096047802353),('B',2192039569602)]

day14b :: String -> Integer
day14b = uncurry (-) . (maximum &&& minimum) . postProcess . (!!40) . solve2 . first preProcess . parseInput

postProcess :: (Integral b, Ord k) => Map.Map (k, k) b -> Map.Map k b
postProcess m = Map.map ((`div` 2) . succ) $ Map.fromListWith (+) $ concatMap (\((a,b),n) -> [(a,n),(b,n)]) $ Map.toList m

preProcess :: String -> Map.Map (Char,Char) Integer
preProcess l = Map.fromListWith (+) $ zipWith (\a b -> ((a,b),1)) l (tail l)

solve2 :: (Map.Map (Char,Char) Integer, Map.Map (Char,Char) Char) -> [Map.Map (Char,Char) Integer]
solve2 (c,r) = iterate (step2 r) c

step2 :: Map.Map (Char,Char) Char -> Map.Map (Char,Char) Integer -> Map.Map (Char,Char) Integer
step2 r m = Map.fromListWith (+) $ concatMap foo $ Map.toList m
  where
    foo (p@(a,b),n) = case Map.lookup p r of
      Nothing -> [(p,n)]
      Just x -> [((a,x),n), ((x,b),n)]


-- | Testing day14
-- >>> day14 testInput
-- 1588

-- | Testing solve
-- >>> take 3 $ solve $ parseInput testInput
-- ["NNCB","NCNBCHB","NBCCNBBBCBHCB"]

day14 :: String -> Int
day14 = uncurry (-) . (maximum &&& minimum) . tally . (!!10) . solve . parseInput

solve :: (String, Map.Map (Char, Char) Char) -> [String]
solve (t,r) = iterate (step r) t

tally :: [Char] -> [Int]
tally = map length . group . sort

step :: Map.Map (Char, Char) Char -> String -> String
step r s = concatMap (either pure id) $ interlace s (map foo ps)
  where
  ps = zip s (tail s)
  foo p@(a,b) = maybe [] pure (Map.lookup p r)

interlace :: [a] -> [b] -> [Either a b]
interlace as bs = case (as,bs) of
  ([],bb)      -> map Right bb
  (a:aa, b:bb) -> Left a : Right b : interlace aa bb
  (aa, [])     -> map Left aa

parseInput :: String -> (String, Map.Map (Char,Char) Char)
parseInput = input . splitOn [""] . lines
  where
  input [[a],b] = (a, Map.fromList $ map (rule . splitOn " -> ") b)
  input x = error $ "oops2: " <> show x

  rule [[a,b],[c]] = ((a,b),c)
  rule _ = error "oops"

-- | Testing parseInput
-- >>> parseInput testInput
-- ("NNCB",fromList [(('B','B'),'N'),(('B','C'),'B'),(('B','H'),'H'),(('B','N'),'B'),(('C','B'),'H'),(('C','C'),'N'),(('C','H'),'B'),(('C','N'),'C'),(('H','B'),'C'),(('H','C'),'B'),(('H','H'),'N'),(('H','N'),'C'),(('N','B'),'B'),(('N','C'),'B'),(('N','H'),'C'),(('N','N'),'C')])

testInput :: String
testInput = unlines $ drop 1 $ lines [r|
NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
|]
