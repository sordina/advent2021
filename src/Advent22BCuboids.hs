{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Monad
import Control.Arrow ((&&&))

(+~) :: Cuboid -> Cuboid -> Set.Set Cuboid
a +~ b = case compositeCuboids a b of
    Nothing    -> Set.fromList [a,b]
    Just (s,t) -> Set.union s t

(-~) :: Cuboid -> Cuboid -> Set.Set Cuboid
a -~ b = case compositeCuboids a b of
    Nothing    -> Set.singleton a
    Just (s,t) -> s Set.\\ t

-- | Calculates the volume of a cuboid.
-- >>> volumeM (Cuboid [])
-- >>> volumeM (Cuboid [(1,1)])
-- >>> volumeM (Cuboid [(1,2), (3,4)])
-- >>> volumeM (Cuboid [(1,2), (3,4), (5,6)])
-- 1
-- 1
-- 4
-- 8
volume :: Cuboid -> Int
volume (Cuboid rs) = product $ map (succ . uncurry (-) . swap) rs

-- | Finds the intersection of two N-dimensional voxel coordinate cuboids
-- >>> intersectCuboids (Cuboid [(1,3)]) (Cuboid [(2,4)]) -- Linear overlap 1d
-- >>> intersectCuboids (Cuboid [(1,3),(1,3)]) (Cuboid [(2,4),(4,9)]) -- Independent on one dimension 2d
-- >>> intersectCuboids (Cuboid [(1,3),(1,3)]) (Cuboid [(2,4),(2,9)]) -- Corners overlapping 2d
-- >>> intersectCuboids (Cuboid [(1,3),(1,3),(1,2)]) (Cuboid [(2,2),(2,2),(0,4)]) -- Skewered 3d
-- >>> intersectCuboids (Cuboid [(0,3),(0,3),(0,3)]) (Cuboid [(2,4),(2,4),(2,4)]) -- Corners overlapping 3d
-- >>> intersectCuboids (Cuboid [(0,3),(0,3),(0,3)]) (Cuboid [(2,4),(2,4),(4,5)]) -- Independent on one dimension 3d
intersectCuboids :: Cuboid -> Cuboid -> Maybe Cuboid
intersectCuboids (Cuboid rs1) (Cuboid rs2) = Cuboid <$> zipWithM intersectR rs1 rs2

-- | The regions represented by sub-cuboids that belong to cube A and cube B
--   the intersecting region is part of both sets.
-- 
--   The function exists in this form so that it can be used for subsequent
--   addition or subtraction operations.
compositeCuboids :: Cuboid -> Cuboid -> Maybe (Set.Set Cuboid, Set.Set Cuboid)
compositeCuboids (Cuboid rs1) (Cuboid rs2) = (possible gl &&& possible gr) <$> zipWithM compositeR rs1 rs2
    where
    gl (Nothing, c, _      ) = [c]
    gl (Just l,  c, _      ) = [l,c]
    gr (_,       c, Nothing) = [c]
    gr (_,       c, Just r ) = [c,r]
    possible f = Set.fromList . map Cuboid . mapM f

-- Splits an intersection into mutually exclusive regions and intersection (a independent, intersecting, b independent)
compositeR :: P2 -> P2 -> Maybe (Maybe P2, P2, Maybe P2)
compositeR (a1,a2) (b1,b2)
    | a1 <= b1 && a2 >= b1 && a2 <= b2 = mutex (a1,b1) (b1,a2) (a2,b2) -- a overlapping b from the left
    | b1 <= a1 && b2 >= a1 && b2 <= a2 = mutex (b1,a1) (a1,b2) (b2,a2) -- a overlapping b from the right
    | a1 >= b1 && a2 <= b2             = mutex (b1,a1) (a1,a2) (a2,b2) -- a inside b
    | b1 >= a1 && b2 <= a2             = mutex (a1,b1) (b1,b2) (b2,a2) -- b inside a
    | otherwise                        = Nothing -- independent

-- | Removes the mutually exclusive regions if they don't extend the intersection
mutex :: (Int, Int) -> (a, b) -> (Int, Int) -> Maybe (Maybe (Int, Int), (a, b), Maybe (Int, Int))
mutex (a1,a2) (b1,b2) (c1,c2) = Just (significant a1 (pred a2), (b1,b2), significant (succ c1) c2)

-- Creates significant (non-zero) regions
significant :: Int -> Int -> Maybe (Int,Int)
significant a b
    | a < b = Just (a,b)
    | otherwise = Nothing

-- Nothing represents non-intersection
intersectR :: P2 -> P2 -> Maybe P2
intersectR (a1,a2) (b1,b2)
    | a1 <= b1 && a2 >= b1 && a2 <= b2 = Just (b1,a2) -- a overlapping b from the left
    | b1 <= a1 && b2 >= a1 && b2 <= a2 = Just (a1,b2) -- a overlapping b from the right
    | a1 >= b1 && a2 <= b2 = Just (a1,a2) -- a inside b
    | b1 >= a1 && b2 <= a2 = Just (b1,b2) -- b inside a
    | otherwise = Nothing -- independent

-- Argument is the range in each dimension
newtype Cuboid = Cuboid [P2]
    deriving (Eq, Ord, Show)

type P2 = (Int,Int)
