{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- Remember that coordinates represent 'voxels' here, not edges,
-- therefore, [(1,1),(1,1),(1,1)] has a volume of 1
-- and the intersection of [(1,1),(1,1),(1,1)] and [(1,1),(1,1),(1,1)] is [(1,1),(1,1),(1,1)]

-- | Mini library for working with cuboids.
--   Main logic for problem 22b is related to working with sets of cuboids and lives in Advent22b
module Advent22BCuboids where

import Data.Set qualified as Set
import Data.Tuple (swap)
import Control.Monad ( zipWithM )
import Control.Arrow ((&&&))
import Data.Maybe (catMaybes)

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
-- Just ([(0,0)],(1,1),[(2,2)])
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
