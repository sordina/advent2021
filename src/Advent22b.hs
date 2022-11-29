{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

-- | Run with `cat data/day22.input.small | cabal run advent2021 22b`
-- 
-- Other cool libraries:
-- * https://github.com/ChrisPenner/grids#grids
-- * https://hackage.haskell.org/package/grids-0.5.0.1/docs/Data-Grid-Examples-Conway.html
-- * https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html
module Advent22b where

import Advent22 qualified as A22
import Data.Set qualified as Set
import Text.RawString.QQ qualified as Q
import Data.Vector qualified as V
import Data.Foldable (foldl', toList)
import Data.Tuple (swap)
import Control.Arrow ((&&&), Arrow (second))
import Data.Maybe (catMaybes)
import GHC.TypeNats (Nat, natVal, KnownNat)
import Data.Data (Proxy(..))
import Data.List (intercalate)

day22b :: String -> Int
day22b = sum . map volume . Set.toList . process . map cubify . A22.parseInput

process :: [(Bool, Cuboid n)] -> Set.Set (Cuboid n)
process = foldl' apply Set.empty

-- | Testing a very simple 2d inputs
-- >>> process [(True, mkVec2 ((0,3),(0,3)))]
-- >>> process [(True, mkVec2 ((0,3),(0,3))), (False, mkVec2 ((3,4),(3,4)))]
-- >>> process [(True, mkVec2 ((0,3),(0,3))), (True, mkVec2  ((3,6),(3,6)))]
-- >>> process [(True, mkVec2 ((0,3),(0,3))), (True, mkVec2  ((3,6),(3,6))), (False, mkVec2 ((3,3),(3,3)))]

apply :: Set.Set (Cuboid n) -> (Bool, Cuboid n) -> Set.Set (Cuboid n)
apply s (False, c) = Set.unions $ Set.map (-~ c) s -- EZ
apply s (True,  c) = Set.insert c $ Set.unions $ Set.map (-~ c) s -- Why not?? :D

cubify :: (a, (P2, P2, P2)) -> (a, Cuboid 3)
cubify = second mkVec3

-- | Testing day22 on testInput
-- >>> day22b testInputSmall
-- 39

-- | Testing parseInput on testInput
-- >>> cubify <$> A22.parseInput testInputSmall
-- [(True,V3 <(10,12),(10,12),(10,12)>),(True,V3 <(11,13),(11,13),(11,13)>),(False,V3 <(9,11),(9,11),(9,11)>),(True,V3 <(10,10),(10,10),(10,10)>)]

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
(-~) :: Cuboid n -> Cuboid n -> Set.Set (Cuboid n)
a -~ b = case compositeCuboids a b of
    Nothing    -> Set.singleton a
    Just (s,t) -> s Set.\\ t

-- | Calculates the volume of a cuboid.
-- >>> volume <$> mkVec @0 []
-- >>> volume <$> mkVec @1 [(1,1)]
-- >>> volume <$> mkVec @2 [(1,2), (3,4)]
-- >>> volume <$> mkVec @3 [(1,2), (3,4), (5,6)]
volume :: Cuboid n -> Int
volume rs = product $ fmap (succ . uncurry (-) . swap) rs

-- | The regions represented by sub-cuboids that belong to cube A and cube B
--   the intersecting region is part of both sets.
-- 
--   The function exists in this form so that it can be used for subsequent
--   addition or subtraction operations.
compositeCuboids :: Cuboid n -> Cuboid n -> Maybe (Set.Set (Cuboid n), Set.Set (Cuboid n))
compositeCuboids rs1 rs2 = (possible gl &&& possible gr) <$> zipWithMVec compositeR rs1 rs2
    where
    gl (l, c, _) = c:l
    gr (_, c, r) = c:r
    possible f = Set.fromList . toList . mapM f

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

-- Data Types and constructors

zipWithMVec :: Monad m => (a -> b -> m c) -> Vec n a -> Vec n b -> m (Vec n c)
zipWithMVec f (UnsafeMkVec a) (UnsafeMkVec b) = UnsafeMkVec <$> V.zipWithM f a b

mkVec :: forall n a. KnownNat n => [a] -> Maybe (Vec n a)
mkVec l
    | n == length l = Just (UnsafeMkVec (V.fromListN n l))
    | otherwise = Nothing
    where
    n = fromIntegral (natVal (Proxy @n))

mkVec2 :: (a,a) -> Vec 2 a
mkVec2 (a,b) = UnsafeMkVec (V.fromListN 2 [a,b])

mkVec3 :: (a,a,a) -> Vec 3 a
mkVec3 (a,b,c) = UnsafeMkVec (V.fromListN 3 [a,b,c])

-- Elements are ranges in each dimension
type Cuboid (n :: Nat) = Vec n P2

type P2 = (Int,Int)

newtype Vec (n :: Nat) a = UnsafeMkVec { getVector :: V.Vector a }
    deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (Vec n a) where
    show :: Show a => Vec n a -> String
    show v = "V" <> show (length v) <> " <" <> intercalate "," (toList (fmap show v)) <> ">"
