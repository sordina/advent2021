{-# LANGUAGE TypeApplications #-}

module Advent01b where

{-

--- Day 1: Sonar Sweep ---
--- Part Two ---

Considering every single measurement isn't as useful as you expected: there's just too much noise in the data.

Instead, consider sums of a three-measurement sliding window. Again considering the above example:

199  A      
200  A B    
208  A B C  
210    B C D
200  E   C D
207  E F   D
240  E F G  
269    F G H
260      G H
263        H
Start by comparing the first and second three-measurement windows. The measurements in the first window are marked A (199, 200, 208); their sum is 199 + 200 + 208 = 607. The second window is marked B (200, 208, 210); its sum is 618. The sum of measurements in the second window is larger than the sum of the first, so this first comparison increased.

Your goal now is to count the number of times the sum of measurements in this sliding window increases from the previous sum. So, compare A with B, then compare B with C, then C with D, and so on. Stop when there aren't enough measurements left to create a new three-measurement sum.

In the above example, the sum of each three-measurement window is as follows:

A: 607 (N/A - no previous sum)
B: 618 (increased)
C: 618 (no change)
D: 617 (decreased)
E: 647 (increased)
F: 716 (increased)
G: 769 (increased)
H: 792 (increased)
In this example, there are 5 sums that are larger than the previous sum.

Consider sums of a three-measurement sliding window. How many sums are larger than the previous sum?

-}

import Data.List (tails)

day1b :: String -> Int
day1b = solution . map (read @Integer) . words

window :: Int -> [a] -> [[a]]
window n ls = map (take n) (tails ls)

test_input :: [Integer]
test_input = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

test_solution :: Int
test_solution = solution test_input

-- >>> test_solution
-- 5

solution :: [Integer] -> Int
solution = greater . map sum . window 3

greater :: Ord a => [a] -> Int
greater ds = length $ filter id $ zipWith (<) ds (tail ds)
