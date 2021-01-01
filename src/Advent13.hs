module Advent13 where

day13 = undefined
day13b = undefined

-- hoe2 -m Control.Monad.State '
-- (\(a,(b,c)) -> b * mod c a)
-- .
-- (id *** minimumBy (compare `on` snd))
-- .
-- (\(x,y) -> (x,) $ map (\z -> (z,) $ head $ dropWhile (<x) $ iterate (+z) 0) y)
-- .
-- (id *** map read . words)
-- .
-- (read.(!!0) &&& concatMap (\x-> if elem x "x," then " " else [x]).(!!1))
-- .
-- lines
-- '

-- #!/bin/sh

-- # https://www.reddit.com/r/learnprogramming/comments/7bcw31/least_common_multiple_with_an_offset/
-- # https://math.stackexchange.com/questions/2218763/how-to-find-lcm-of-two-numbers-when-one-starts-with-an-offset
-- # https://en.wikipedia.org/wiki/Least_common_multiple#Calculation
-- # https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
-- # https://en.wikipedia.org/wiki/Bézout%27s_identity
-- # https://en.wikipedia.org/wiki/Polynomial_greatest_common_divisor#Bézout's_identity_and_extended_GCD_algorithm

-- hoe2 -m 'Math.NumberTheory.Moduli.Class Math.NumberTheory.Moduli.Chinese' '
-- 	uncurry (-)
-- 	.
-- 	(read.init.last &&& read.tail.head) . words . show
-- 	.
-- 	foldl1 (\a b -> fromJust $ chineseSomeMod a b)
-- 	.
-- 	map (uncurry modulo)
-- 	.
-- 	map (id *** read)
-- 	.
-- 	filter ((/="x").snd) . zip [0..] .  words
-- 	.
-- 	concatMap (\x-> if elem x "," then " " else [x])
-- 	.
-- 	(!!1) .  lines
-- '
