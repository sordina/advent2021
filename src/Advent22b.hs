{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | Run with `cat data/day22.input.small | cabal run advent2021 22b`
module Advent22b where

import Advent22 qualified as A22
import Data.Set qualified as Set
import Text.RawString.QQ qualified as Q
import Data.Foldable (foldl')
import Data.Tuple (swap)
import Control.Monad ( zipWithM )
import Control.Arrow ((&&&))
import Data.Maybe (catMaybes)

day22b :: String -> Int
day22b = sum . map volume . Set.toList . process . map cubify . A22.parseInput

process :: [(Bool, Cuboid)] -> Set.Set Cuboid
process = foldl' apply Set.empty

-- | Testing a very simple 2d inputs
-- >>> process [(True, Cuboid [(0,3),(0,3)])]
-- >>> process [(True, Cuboid [(0,3),(0,3)]), (False, Cuboid [(3,4),(3,4)])]
-- >>> process [(True, Cuboid [(0,3),(0,3)]), (True, Cuboid [(3,6),(3,6)])]
-- >>> process [(True, Cuboid [(0,3),(0,3)]), (True, Cuboid [(3,6),(3,6)]), (False, Cuboid [(3,3),(3,3)])]
-- fromList [Cuboid [(0,3),(0,3)]]
-- fromList [Cuboid [(0,2),(0,2)],Cuboid [(0,2),(3,3)],Cuboid [(3,3),(0,2)]]
-- fromList [Cuboid [(0,2),(0,2)],Cuboid [(0,2),(3,3)],Cuboid [(3,3),(0,2)],Cuboid [(3,6),(3,6)]]
-- fromList [Cuboid [(0,2),(0,2)],Cuboid [(0,2),(3,3)],Cuboid [(3,3),(0,2)],Cuboid [(3,3),(4,6)],Cuboid [(4,6),(3,3)],Cuboid [(4,6),(4,6)]]

apply :: Set.Set Cuboid -> (Bool, Cuboid) -> Set.Set Cuboid
apply s (False, c) = Set.unions $ Set.map (-~ c) s -- EZ
apply s (True,  c) = Set.insert c $ Set.unions $ Set.map (-~ c) s -- Why not?? :D

cubify :: (a, (P2, P2, P2)) -> (a, Cuboid)
cubify (b, (x,y,z)) = (b, Cuboid [x,y,z])

-- | Testing day22 on testInput
-- >>> day22b testInputSmall
-- 39

-- | Testing parseInput on testInput
-- >>> A22.parseInput testInputSmall
-- [(True,((10,12),(10,12),(10,12))),(True,((11,13),(11,13),(11,13))),(False,((9,11),(9,11),(9,11))),(True,((10,10),(10,10),(10,10)))]

testInputSmall :: String
testInputSmall = unlines $ tail $ lines [Q.r|
on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10
|]

--------------------------------------------------

-- * Operations for working with cuboids
-- * Remember that coordinates represent 'voxels' here, not edges,
-- * therefore, [(1,1),(1,1),(1,1)] has a volume of 1
-- * and the intersection of [(1,1),(1,1),(1,1)] and [(1,1),(1,1),(1,1)] is [(1,1),(1,1),(1,1)]

-- | A -~ B Subtracts B from A
--   It seems that this should be able to be accomplished by just taking the left hand side
--   of a composite operation, however this yields the wrong value.
--   Investigation into why this is the case would be worth while.
-- 
(-~) :: Cuboid -> Cuboid -> Set.Set Cuboid
a -~ b = case compositeCuboids a b of
    Nothing    -> Set.singleton a
    Just (s,t) -> s Set.\\ t

-- | Calculates the volume of a cuboid.
-- >>> volume (Cuboid [])
-- >>> volume (Cuboid [(1,1)])
-- >>> volume (Cuboid [(1,2), (3,4)])
-- >>> volume (Cuboid [(1,2), (3,4), (5,6)])
-- 1
-- 1
-- 4
-- 8
volume :: Cuboid -> Int
volume (Cuboid rs) = product $ map (succ . uncurry (-) . swap) rs

-- | The regions represented by sub-cuboids that belong to cube A and cube B
--   the intersecting region is part of both sets.
-- 
--   The function exists in this form so that it can be used for subsequent
--   addition or subtraction operations.
compositeCuboids :: Cuboid -> Cuboid -> Maybe (Set.Set Cuboid, Set.Set Cuboid)
compositeCuboids (Cuboid rs1) (Cuboid rs2) = (possible gl &&& possible gr) <$> zipWithM compositeR rs1 rs2
    where
    gl (l, c, _) = c:l
    gr (_, c, r) = c:r
    possible f = Set.fromList . map Cuboid . mapM f

-- Splits an intersection into mutually exclusive regions and intersection (a independent, intersecting, b independent)
-- 
-- >>> compositeR (0,1) (2,3) -- Independent
-- >>> compositeR (0,1) (0,1) -- Identical
-- >>> compositeR (0,1) (1,2) -- Partial overlap
-- >>> compositeR (1,2) (0,1) -- Partial overlap, other direction
-- >>> compositeR (1,2) (0,3) -- Subsumption
-- >>> compositeR (0,3) (1,2) -- Subsumption, other direction
-- Nothing
-- Just ([],(0,1),[])
-- Just ([(0,0)],(1,1),[(2,2)])
-- Just ([(2,2)],(1,1),[(0,0)])
-- Just ([],(1,2),[(0,0),(3,3)])
-- Just ([(0,0),(3,3)],(1,2),[])
-- 
compositeR :: P2 -> P2 -> Maybe ([P2], P2, [P2])
compositeR (a1,a2) (b1,b2)
    | a1 <= b1 && a2 >= b1 && a2 <= b2 = Just (s a1 (pred b1), (b1,a2), s (succ a2) b2) -- a overlapping b from the left
    | b1 <= a1 && b2 >= a1 && b2 <= a2 = Just (s (succ b2) a2, (a1,b2), s b1 (pred a1)) -- a overlapping b from the right
    | a1 >= b1 && a2 <= b2             = Just ([], (a1,a2), s b1 (pred a1) ++ s (succ a2) b2) -- a inside b
    | b1 >= a1 && b2 <= a2             = Just (s a1 (pred b1) ++ s (succ b2) a2, (b1,b2), []) -- b inside a
    | otherwise                        = Nothing -- independent
    where
    s x y = catMaybes [significant x y]

-- Creates significant (non-zero) regions
significant :: Int -> Int -> Maybe (Int,Int)
significant a b
    | a <= b = Just (a,b)
    | otherwise = Nothing

-- Argument is the range in each dimension
newtype Cuboid = Cuboid [P2]
    deriving (Eq, Ord, Show)

type P2 = (Int,Int)