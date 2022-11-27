{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Advent22b where

import Advent22 qualified as A22
import Advent22BCuboids qualified as A22BCuboids
import Data.Set qualified as Set
import Text.RawString.QQ (r)
import Data.Maybe (mapMaybe)
import Control.Monad ((<=<))

day22b :: String -> Int
day22b = sum . map volume . Set.toList . foldl insert Set.empty . map instruct . A22.parseInput

type Point = (Int,Int,Int)
type Instruction = (Bool, Cuboid)
data Cuboid = Cuboid
    { x :: (Int, Int)
    , y :: (Int, Int)
    , z :: (Int, Int)
    }
    deriving (Eq,Ord,Show)

instruct :: (Bool, ((Int, Int), (Int, Int), (Int, Int))) -> Instruction
instruct (b,(x,y,z)) = (b, Cuboid x y z)

volume :: Cuboid -> Int
volume (Cuboid (x1,x2) (y1,y2) (z1,z2)) = (x2-x1) * (y2-y1) * (z2-z1)

-- | Testing add
-- >>> add (True, Cuboid (0,1) (0,1) (0,1)) (Cuboid (1,2) (1,2) (1,2))
-- fromList [Cuboid {x = (1,2), y = (1,2), z = (1,2)}]

between :: Ord a => a -> (a,a) -> Bool
between a (a1,a2) = a > a1 && a < a2

inside :: Cuboid -> Point -> Bool
inside (Cuboid x' y' z') (x,y,z) = between x x' && between y y' && between z z'

rangeToPair :: (Int,Int) -> [Int]
rangeToPair (a,b) = [a,b]

-- | testing points
-- >>> points (Cuboid (0,1) (2,3) (4,5))
-- fromList [(0,2,4),(0,2,5),(0,3,4),(0,3,5),(1,2,4),(1,2,5),(1,3,4),(1,3,5)]

points :: Cuboid -> Set.Set Point
points (Cuboid (rangeToPair -> x) (rangeToPair -> y) (rangeToPair -> z))
    = Set.fromList [(x',y',z') | x' <- x, y' <- y, z' <- z]

-- | Testing overlap
-- >>> overlap (Cuboid (0,2) (0,2) (0,2)) (Cuboid (1,3) (1,3) (1,3))
-- (fromList [(1,1,1)],fromList [(2,2,2)])

overlap :: Cuboid -> Cuboid -> (Set.Set Point, Set.Set Point)
overlap a b = (Set.filter (inside a) (points b), Set.filter (inside b) (points a))

-- | testing cornersToCuboid
-- >>> cornersToCuboid (1,2,3) (4,5,6)
-- Cuboid {x = (1,4), y = (2,5), z = (3,6)}

cornersToCuboid :: Point -> Point -> Cuboid
cornersToCuboid (x1,y1,z1) (x2,y2,z2) = Cuboid (min x1 x2, max x1 x2) (min y1 y2, max y1 y2) (min z1 z2, max z1 z2)

cut :: Cuboid -> Point -> Set.Set Cuboid
cut c p = Set.map (cornersToCuboid p) (points c)

add :: Instruction -> Cuboid -> Set.Set Cuboid
add (t,a) b = undefined
    where
    (pa,pb) = overlap a b
    csa = Set.map (cut a) pa -- TODO
    csb = Set.map (cut b) pb -- TODO

insert :: Set.Set Cuboid -> Instruction -> Set.Set Cuboid
insert s i@(b,a)
    | Set.null s = Set.unions $ Set.map (add i) s
    | otherwise = Set.singleton a -- TODO

-- | Testing day22 on testInput
-- >>> day22b testInput
-- 0

-- | Testing parseInput on testInput
-- >>> A22.parseInput testInput
-- [(True,((10,12),(10,12),(10,12))),(True,((11,13),(11,13),(11,13))),(False,((9,11),(9,11),(9,11))),(True,((10,10),(10,10),(10,10)))]

testInput :: String
testInput = unlines $ tail $ lines [r|
on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10
|]
