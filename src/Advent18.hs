{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NegativeLiterals #-}

module Advent18 where


{-

--- Day 18: Snailfish ---

You descend into the ocean trench and encounter some snailfish. They say they saw the sleigh keys! They'll even tell you which direction the keys went if you help one of the smaller snailfish with his math homework.

Snailfish numbers aren't like regular numbers. Instead, every snailfish number is a pair - an ordered list of two elements. Each element of the pair can be either a regular number or another pair.

Pairs are written as [x,y], where x and y are the elements within the pair. Here are some example snailfish numbers, one snailfish number per line:

[1,2]
[[1,2],3]
[9,[8,7]]
[[1,9],[8,5]]
[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]

This snailfish homework is about addition. To add two snailfish numbers, form a pair from the left and right parameters of the addition operator. For example, [1,2] + [[3,4],5] becomes [[1,2],[[3,4],5]].

There's only one problem: snailfish numbers must always be reduced, and the process of adding two snailfish numbers can result in snailfish numbers that need to be reduced.

To reduce a snailfish number, you must repeatedly do the first action in this list that applies to the snailfish number:

If any pair is nested inside four pairs, the leftmost such pair explodes.
If any regular number is 10 or greater, the leftmost such regular number splits.
Once no action in the above list applies, the snailfish number is reduced.

During reduction, at most one action applies, after which the process returns to the top of the list of actions. For example, if split produces a pair that meets the explode criteria, that pair explodes before other splits occur.

To explode a pair, the pair's left value is added to the first regular number to the left of the exploding pair (if any), and the pair's right value is added to the first regular number to the right of the exploding pair (if any). Exploding pairs will always consist of two regular numbers. Then, the entire exploding pair is replaced with the regular number 0.

Here are some examples of a single explode action:

[[[[[9,8],1],2],3],4] becomes [[[[0,9],2],3],4] (the 9 has no regular number to its left, so it is not added to any regular number).
[7,[6,[5,[4,[3,2]]]]] becomes [7,[6,[5,[7,0]]]] (the 2 has no regular number to its right, and so it is not added to any regular number).
[[6,[5,[4,[3,2]]]],1] becomes [[6,[5,[7,0]]],3].
[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]] becomes [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] (the pair [3,2] is unaffected because the pair [7,3] is further to the left; [3,2] would explode on the next action).
[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] becomes [[3,[2,[8,0]]],[9,[5,[7,0]]]].

To split a regular number, replace it with a pair; the left element of the pair should be the regular number divided by two and rounded down, while the right element of the pair should be the regular number divided by two and rounded up. For example, 10 becomes [5,5], 11 becomes [5,6], 12 becomes [6,6], and so on.

Here is the process of finding the reduced result of [[[[4,3],4],4],[7,[[8,4],9]]] + [1,1]:

after addition: [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]
after explode:  [[[[0,7],4],[7,[[8,4],9]]],[1,1]]
after explode:  [[[[0,7],4],[15,[0,13]]],[1,1]]
after split:    [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
after split:    [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]
after explode:  [[[[0,7],4],[[7,8],[6,0]]],[8,1]]

Once no reduce actions apply, the snailfish number that remains is the actual result of the addition operation: [[[[0,7],4],[[7,8],[6,0]]],[8,1]].

The homework assignment involves adding up a list of snailfish numbers (your puzzle input). The snailfish numbers are each listed on a separate line. Add the first snailfish number and the second, then add that result and the third, then add that result and the fourth, and so on until all numbers in the list have been used once.

For example, the final sum of this list is [[[[1,1],[2,2]],[3,3]],[4,4]]:

[1,1]
[2,2]
[3,3]
[4,4]

The final sum of this list is [[[[3,0],[5,3]],[4,4]],[5,5]]:

[1,1]
[2,2]
[3,3]
[4,4]
[5,5]

The final sum of this list is [[[[5,0],[7,4]],[5,5]],[6,6]]:

[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]

Here's a slightly larger example:

[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]

The final sum [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]] is found after adding up the above snailfish numbers:

  [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
+ [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
= [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]

  [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
+ [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
= [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]

  [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]
+ [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
= [[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]

  [[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]
+ [7,[5,[[3,8],[1,4]]]]
= [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]

  [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]
+ [[2,[2,2]],[8,[8,1]]]
= [[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]

  [[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]
+ [2,9]
= [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]

  [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]
+ [1,[[[9,3],9],[[9,0],[0,7]]]]
= [[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]

  [[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]
+ [[[5,[7,4]],7],1]
= [[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]

  [[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]
+ [[[[4,2],2],6],[8,7]]
= [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]

To check whether it's the right answer, the snailfish teacher only checks the magnitude of the final sum. The magnitude of a pair is 3 times the magnitude of its left element plus 2 times the magnitude of its right element. The magnitude of a regular number is just that number.

For example, the magnitude of [9,1] is 3*9 + 2*1 = 29; the magnitude of [1,9] is 3*1 + 2*9 = 21. Magnitude calculations are recursive: the magnitude of [[9,1],[1,9]] is 3*29 + 2*21 = 129.

Here are a few more magnitude examples:

[[1,2],[[3,4],5]] becomes 143.
[[[[0,7],4],[[7,8],[6,0]]],[8,1]] becomes 1384.
[[[[1,1],[2,2]],[3,3]],[4,4]] becomes 445.
[[[[3,0],[5,3]],[4,4]],[5,5]] becomes 791.
[[[[5,0],[7,4]],[5,5]],[6,6]] becomes 1137.
[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]] becomes 3488.

So, given this example homework assignment:

[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]

The final sum is:

[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]

The magnitude of this final sum is 4140.

Add up all of the snailfish numbers from the homework assignment in the order they appear. What is the magnitude of the final sum?

-}
import Control.Arrow ((&&&), Arrow (first))
import Utils (fixEq)
import Text.RawString.QQ (r)
import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.List (find, isPrefixOf)
import Debug.Trace (traceShow, traceShowId, trace)

import Text.ParserCombinators.ReadP qualified as P

data Pair = Pair Val Val          deriving Eq
data Val  = VNum Int | VPair Pair deriving Eq

instance Show Pair where show (Pair l r) = "[" <> show l <> "," <> show r <> "]"
instance Show Val  where
  show (VNum  i) = show i
  show (VPair p) = show p


-- | Testing day18
-- >>> day18 testInput
-- 4140

day18 :: String -> Int
day18 = magnitude . foldl1 addReduce . parseInput

-- | Testing more examples:
-- >>> let p1 = head $ parsePair "[[[[4,3],4],4],[7,[[8,4],9]]]"
-- >>> let p2 = head $ parsePair "[1,1]"
-- 
-- >>> let p3 = addPair p1 p2
-- >>> p3
-- [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]
-- 
-- >>> let p4 = nesting p3
-- >>> p4
-- [[[[0,7],4],[7,[[8,4],9]]],[1,1]]
-- 
-- >>> let p5 = nesting p4
-- >>> p5
-- [[[[0,7],4],[15,[0,13]]],[1,1]]
-- 
-- >>> let p6 = oversize p5
-- >>> p6
-- [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
-- 
-- >>> let p7 = oversize p6
-- >>> p7
-- [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]
-- 
-- >>> let p8 = nesting p7
-- >>> p8
-- [[[[0,7],4],[[7,8],[6,0]]],[8,1]]

-- | Testing addReduce
-- >>> foldl1 addReduce . parseInput $ testInput
-- [[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]

magnitude :: Pair -> Int
magnitude (Pair l r) = 3 * magnitudeVal l + 2 * magnitudeVal r

-- | Testing magnitude examples:
-- >>> map magnitude (parsePair "[[1,2],[[3,4],5]]")
-- [143]
-- >>> map magnitude (parsePair "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
-- [1384]
-- >>> map magnitude (parsePair "[[[[1,1],[2,2]],[3,3]],[4,4]]")
-- [445]
-- >>> map magnitude (parsePair "[[[[3,0],[5,3]],[4,4]],[5,5]]")
-- [791]
-- >>> map magnitude (parsePair "[[[[5,0],[7,4]],[5,5]],[6,6]]")
-- [1137]
-- >>> map magnitude (parsePair "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")
-- [3488]

magnitudeVal :: Val -> Int
magnitudeVal (VNum n) = n
magnitudeVal (VPair p) = magnitude p

addReduce :: Pair -> Pair -> Pair
addReduce a b = reduce $ addPair a b

-- | Testing addReduce examples:
-- >>> let p1 = head $ parsePair "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
-- >>> let p2 = head $ parsePair "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
-- 
-- >>> addReduce p1 p2
-- [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
-- 
-- >>> let p1 = head $ parsePair "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
-- >>> let p2 = head $ parsePair "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
-- >>> addReduce p1 p2
-- [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]
-- 
-- >>> let p1 = head $ parsePair "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"
-- >>> let p2 = head $ parsePair "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
-- >>> addReduce p1 p2
-- [[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]
-- 
-- >>> let p1 = head $ parsePair "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]"
-- >>> let p2 = head $ parsePair "[7,[5,[[3,8],[1,4]]]]"
-- >>> addReduce p1 p2
-- [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]
-- 
-- >>> let p1 = head $ parsePair "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]"
-- >>> let p2 = head $ parsePair "[[2,[2,2]],[8,[8,1]]]"
-- >>> addReduce p1 p2
-- [[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]
-- 
-- >>> let p1 = head $ parsePair "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]"
-- >>> let p2 = head $ parsePair "[2,9]"
-- >>> addReduce p1 p2
-- [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]
-- 
-- >>> let p1 = head $ parsePair "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"
-- >>> let p2 = head $ parsePair "[1,[[[9,3],9],[[9,0],[0,7]]]]"
-- >>> addReduce p1 p2
-- [[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]
-- 
-- >>> let p1 = head $ parsePair "[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]"
-- >>> let p2 = head $ parsePair "[[[5,[7,4]],7],1]"
-- >>> addReduce p1 p2
-- [[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]
-- 
-- >>> let p1 = head $ parsePair "[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]"
-- >>> let p2 = head $ parsePair "[[[[4,2],2],6],[8,7]]"
-- >>> addReduce p1 p2
-- [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]

addPair :: Pair -> Pair -> Pair
addPair p1 p2 = Pair (VPair p1) (VPair p2)

-- | Testing addPair
-- >>> foldl1 addPair (concatMap parsePair ["[1,2]","[3,4]"])
-- [[1,2],[3,4]]
-- 
-- >>> foldl1 addPair (concatMap parsePair ["[1,2]","[3,4]","[[5,6],7]"])
-- [[[1,2],[3,4]],[[5,6],7]]

-- | Testing addReduce
-- >>> foldl1 addReduce (concatMap parsePair ["[1,20]","[3,4]"])
-- [[1,[[5,5],[5,5]]],[3,4]]
-- 
-- >>> reduce $ head $ parsePair "[[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]],[[[5,[2,8]],4],[5,[[9,9],0]]]]"
-- [[[[7,0],[7,8]],[[7,9],[0,6]]],[[[7,0],[6,6]],[[7,7],[0,9]]]]
-- 
-- >>> fixEq ((oversize `oneOf` nesting)) $ head $ parsePair "[[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]],[[[5,[2,8]],4],[5,[[9,9],0]]]]"
-- [[[[7,7],[6,0]],[[7,6],[5,6]]],[[[7,0],[6,6]],[[7,7],[0,9]]]]

-- >>> nesting $ head $ parsePair "[[[[[[8,7],6],5],4],3],2]"
-- [[[[[0,13],5],4],3],2]
-- >>> map (length . fst) $ label $ head $ parsePair "[[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]],[[[5,[2,8]],4],[5,[[9,9],0]]]]"
-- >>> map (length . fst) $ label $ head $ parsePair "[[[[5,9],[0,[[8,8],6]]],[[4,[1,2]],[[1,4],2]]],[[[5,[2,8]],4],[5,[[9,9],0]]]]"
-- [4,5,5,5,5,5,5,4,5,5,5,5,4,4,5,5,3,3,5,5,4]
-- [4,4,4,6,6,5,4,5,5,5,5,4,4,5,5,3,3,5,5,4]

reduce :: Pair -> Pair
reduce = fixEq (oneOf nesting oversize)

-- >>> nesting $ head $ parsePair "[[[[[1,2],3],4],5],6]"
-- [[[[0,5],4],5],6]

oneOf :: Eq t => (t -> t) -> (t -> t) -> t -> t
oneOf f g x
  | x /= y = y
  | otherwise = g x
  where
  y = f x

-- | Testing oneOf:
-- >>> oneOf nesting oversize $ head $ parsePair "[1,2]"
-- [1,2]
-- >>> oneOf nesting oversize $ head $ parsePair "[[[[[1,2],3],4],5],6]"
-- [[[[0,5],4],5],6]

oversize :: Pair -> Pair
oversize p = maybe p (splitPairAt p . fst) oversized 
  where
  labels    = labelNums p
  oversized = find ((>=10).snd) labels

-- | Testing oversize examples (split):
-- >>> oversize $ head $ parsePair "[[[[0,7],4],[15,[0,13]]],[1,1]]"
-- [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
-- >>> oversize $ head $ parsePair "[1,2]"
-- [1,2]
-- >>> oversize $ head $ parsePair "[[1,2],[3,4]]"
-- [[1,2],[3,4]]

splitPairAt :: Pair -> [Bool] -> Pair
splitPairAt _                 []          = error "splitPairAt exhausted path"
splitPairAt (Pair (VNum n) r) [False]     = Pair (VPair (Pair (VNum (n `div` 2)) (VNum (div2U n)))) r
splitPairAt (Pair l (VNum n)) [True ]     = Pair l (VPair (Pair (VNum (n `div` 2)) (VNum (div2U n))))
splitPairAt (Pair (VPair p) r) (False:bs) = Pair (VPair (splitPairAt p bs)) r
splitPairAt (Pair l (VPair p)) (True: bs) = Pair l (VPair (splitPairAt p bs))
splitPairAt _                  _          = error "splitPairAt mismatched pair"

div2U :: Integral a => a -> a
div2U n | odd n     = 1 + (n `div` 2)
        | otherwise = n `div` 2

-- To split a regular number, replace it with a pair; the left element of the pair should be the regular number divided by two and rounded down, while the right element of the pair should be the regular number divided by two and rounded up. For example, 10 becomes [5,5], 11 becomes [5,6], 12 becomes [6,6], and so on.

nesting :: Pair -> Pair
nesting p = onFirst p (nestingLocation p) explode

-- | Testing nesting:
-- >>> nesting $ head $ parsePair "[[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]],[[[5,[2,8]],4],[5,[[9,9],0]]]]"
-- [[[[5,0],[[9,7],[9,6]]],[[4,[1,2]],[[1,4],2]]],[[[5,[2,8]],4],[5,[[9,9],0]]]]

-- | Testing fixEq nesting
-- >>> fixEq nesting $ head $ parsePair "[[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]],[[[5,[2,8]],4],[5,[[9,9],0]]]]"
-- [[[[5,9],[16,0]],[[11,3],[0,6]]],[[[7,0],12],[14,[0,9]]]]

onFirst :: a -> [b] -> (a -> b -> a) -> a
onFirst x []    _ = x
onFirst x (p:_) f = f x p

nestingLocation :: Pair -> [([Bool],Pair)]
nestingLocation = filter ((>3) . length . fst) . labelNumPairs

explode :: Pair -> ([Bool],Pair) -> Pair
explode p (bs,r) = distribute r bs (labelNums p) $ replaceZero p bs

-- | Testing nesting (explode) examples:
-- >>> labelNumPairs $ head $ parsePair "[[[[[9,8],7],6],5],[[[[4,5],3],2],1]]"
-- [([False,False,False,False],[9,8]),([True,False,False,False],[4,5])]

-- | Testing nesting
-- >>> nesting $ head $ parsePair "[[[[[9,8],1],2],3],4]"
-- [[[[0,9],2],3],4]

distribute :: Pair -> [Bool] -> [([Bool],Int)] -> Pair -> Pair
distribute (Pair (VNum l) (VNum r)) bs ls p = addLeft r bs (reverse ls) $ addLeft l bs ls p
distribute _ _ _ _ = error "distributing non-numeric pair"

addLeft :: Int -> [Bool] -> [([Bool],Int)] -> Pair -> Pair
addLeft n bs labels p
  | null upto = p
  | otherwise = addTo n leftPath p
  where
  upto          = takeWhile (not.(bs `isPrefixOf`).fst) labels
  (leftPath, _) = last upto

addTo :: Int -> [Bool] -> Pair -> Pair
addTo n []         _                  = error "path exhausted"
addTo n [False]    (Pair (VNum x) r)  = Pair (VNum (n + x)) r
addTo n [True]     (Pair l (VNum x))  = Pair l (VNum (n + x))
addTo n (False:bs) (Pair (VPair l) r) = Pair (VPair (addTo n bs l)) r
addTo n (True:bs)  (Pair l (VPair r)) = Pair l (VPair (addTo n bs r))
addTo _ _          _                  = error "mismatch addTo"

labelNums :: Pair -> [([Bool], Int)]
labelNums = map (first reverse) . labelNums' []

labelNums' :: [Bool] -> Pair -> [([Bool], Int)]
labelNums' bs (Pair l r) = labelVal (False : bs) l ++ labelVal (True : bs) r

labelVal :: [Bool] -> Val -> [([Bool], Int)]
labelVal bs (VNum  n) = [(bs, n)]
labelVal bs (VPair p) = labelNums' bs p

labelNumPairs :: Pair -> [([Bool], Pair)]
labelNumPairs = map (first reverse) . labelNumPairs' []

labelNumPairs' :: [Bool] -> Pair -> [([Bool], Pair)]
labelNumPairs' bs p@(Pair (VNum _)  (VNum  _)) = [(bs,p)]
labelNumPairs' bs   (Pair (VNum _)  (VPair x)) = labelNumPairs' (True  : bs) x
labelNumPairs' bs   (Pair (VPair x) (VNum  _)) = labelNumPairs' (False : bs) x
labelNumPairs' bs   (Pair (VPair l) (VPair r)) = labelNumPairs' (False : bs) l ++ labelNumPairs' (True : bs) r


-- >>> map labelNums (parsePair "[1,[[2,3],4]]")
-- [[([False],1),([True,False,False],2),([True,False,True],3),([True,True],4)]]

-- >>> map labelNumPairs (parsePair "[1,[[2,3],4]]")
-- [[([False,False],[2,3])]]

replaceZero :: Pair -> [Bool] -> Pair
replaceZero (Pair (VPair _) r) [False]    = Pair (VNum 0) r
replaceZero (Pair l (VPair _)) [True ]    = Pair l (VNum 0)
replaceZero (Pair (VPair p) r) (False:xs) = Pair (VPair (replaceZero p xs)) r
replaceZero (Pair l (VPair p)) (True :xs) = Pair l (VPair (replaceZero p xs))
replaceZero p                  bs         = error $ "replaceZero - unexpected pattern: " <> show (p,bs)

-- >>> head $ parsePair "[[[[[1,2],3],4],5],6]"
-- >>> flip replaceZero [False,False,False,False] $ head $ parsePair "[[[[[1,2],3],4],5],6]"
-- [[[[[1,2],3],4],5],6]
-- [[[[0,3],4],5],6]

parseInput :: String -> [Pair]
parseInput = concatMap parsePair . lines

parsePair :: String -> [Pair]
parsePair = take 1 . map fst . filter (null . snd) . P.readP_to_S pairParser

-- | Testing ParsePair
-- >>> parsePair "[1,2]"
-- [[1,2]]

pairParser :: P.ReadP Pair
pairParser = do
  _ <- P.char '['
  l <- valueParser
  _ <- P.char ','
  r <- valueParser
  _ <- P.char ']'
  pure (Pair l r)
  where
  valueParser = (VNum . read <$> P.many1 (P.satisfy isDigit)) <|> (VPair <$> pairParser)

-- | Testing parseInput
-- >>> parseInput testInput
-- [[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]],[[[5,[2,8]],4],[5,[[9,9],0]]],[6,[[[6,2],[5,6]],[[7,6],[4,7]]]],[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]],[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]],[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]],[[[[5,4],[7,7]],8],[[8,3],8]],[[9,3],[[9,9],[6,[4,9]]]],[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]],[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]]
--- >>> and $ zipWith (==) (lines testInput) (map show $ parseInput testInput)
-- True


testInput :: String
testInput = unlines $ tail $ lines [r|
[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
|]
