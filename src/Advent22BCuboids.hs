{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | Mini library for working with cuboids.
--   Main logic for problem 22b is related to working with sets of cuboids and lives in Advent22b
module Advent22BCuboids where

import Advent22 qualified as A22
import Data.Set qualified as Set
import Text.RawString.QQ (r)
import Data.Maybe (mapMaybe)
import Control.Monad ((<=<))


-- The two main entrypoints into this module:

(+~+) :: Cuboid -> Cuboid -> Set.Set Cuboid
c1 +~+ c2 = exec c1 a1 `Set.union` exec c2 a2
    where
    (a1, a2) = analyze c1 c2

(-~-) :: Cuboid -> Cuboid -> Set.Set Cuboid
c1 -~- c2 = undefined

exec :: Cuboid -> Scenario -> Set.Set Cuboid
exec c = \case
    Independent -> Set.singleton c
    CornersTraded p -> splitCuboidByPoint p c
    FaceDipped ps -> splitCuboidByPoints ps c
    Swallows i -> splitCuboidByPoints (corners i) c
    SubScenario2d s -> case s of
        Independent2d -> Set.singleton c
        EdgesTraded p -> undefined
        Chiseled s -> undefined


-- Scenarios to consider for finding the intersection of two cuboids:
-- * The two cuboids are independent
-- * A point from one cuboid is inside the other (easily tested via counting)
--   + Either two cubes trade one of each other's corners
--   + or a face protrudes inside a larger cuboid
--   + or one cuboid swallows another
-- * An edge from one cuboid pierces the other
--   ... This can be approached via a combination of 3d->2d dimension elimination
--   ... with the constraint that 1 cuboid must swallow the other in the discarded dimension.
--   ... We only need to consider 1 viable candidate for a discarded dimension, since
--   ...... If 2 dimensions are removed then the result will be a zero-volume cube
--   ...... If more than one candidate is viable, then there would have been a point traded somewhere
--   + Either two edges are traded diagonally
--   + or one cuboid skerwers the other
--   + or one cuboid chisels the other

data Scenario
    = Independent
    | CornersTraded Point
    | FaceDipped [Point]
    | Swallows Cuboid
    | SubScenario2d Scenario2d -- Should this be 'reinterpreted' back to a 3d scenario?
    deriving (Eq, Show)

data Scenario2d
    = Independent2d
    | EdgesTraded Point2d
    | Chiseled Segment
    deriving (Eq, Show)

-- | Find the scenarios described
--   The pair represents the scenarios corresponding to the cuboids analyzed,
--   With the scenario's points appearing inside the corresponding cube.
--   This approach is taken so that the cubes can easily be split by the relevant points contained within them.
-- 
--   Examples:
-- 
--  >>> analyze ((0,0,0),(3,3,3)) ((1,1,1),(2,2,2))
-- (Swallows ((1,1,1),(2,2,2)),Independent)
analyze :: Cuboid -> Cuboid -> (Scenario, Scenario)
analyze c1 c2
    | null c2cs' && null c1cs' = (Independent, Independent)
    | [c2c] <- c2cs', [c1c] <- c1cs' = (CornersTraded c2c, CornersTraded c1c) -- Pattern guard used. See: https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-460003.13
    | [_,_,_,_] <- c2cs', null c1cs' = (FaceDipped c2cs', Independent)
    | null c2cs', [_,_,_,_] <- c1cs' = (Independent, FaceDipped c1cs')
    | count 8 c2cs' && null c1cs' = (Swallows c2, Independent)
    | null c2cs' && count 8 c1cs' = (Independent, Swallows c1)
    | ((s1,s2):_) <- mapMaybe (analyze2d <=< considerD) d3s = (SubScenario2d s1, SubScenario2d s2) -- Only consider the first viable 2d scenario, since all projections will be equivalent
    where
    c1cs = corners c1
    c2cs = corners c2
    c1cs' = filter (inside c2) c1cs -- c1 corners inside c2
    c2cs' = filter (inside c1) c2cs -- c2 corners inside c1
    considerD d = (d,,) <$> eliminateD d c1 (projectD d c2) <*> eliminateD d c2 (projectD d c1)

-- Performs and analog to analyze but in 2 dimensions
-- Can potentially fail due to non-overlapping rectangles
-- and we don't need another "Independent" scenario for 2d.
analyze2d :: (Dimension, Rectangle, Rectangle) -> Maybe (Scenario2d, Scenario2d)
analyze2d (d,r1,r2)
    | null r2cs' && null r1cs' = Just (Independent2d, Independent2d)
    | [c2c] <- r2cs', [c1c] <- r1cs' = Just (EdgesTraded c2c, EdgesTraded c1c)
    -- TODO: Other scenarios as documented above
    | otherwise = Nothing
    where
    r1cs = corners2d r1
    r2cs = corners2d r2
    r1cs' = filter (inside2d r2) r1cs
    r2cs' = filter (inside2d r1) r2cs

-- | Removes the given dimension from the first cube if the second cube contains it in that dimension
eliminateD :: Dimension -> Cuboid -> Segment -> Maybe Rectangle
eliminateD X ((x1,y1,z1),(x2,y2,z2)) (x1',x2') | x1 > x1' && x2 < x2' = Just ((y1,z1),(y2,z2))
eliminateD Y ((x1,y1,z1),(x2,y2,z2)) (y1',y2') | y1 > y1' && y2 < y2' = Just ((x1,z1),(x2,z2))
eliminateD Z ((x1,y1,z1),(x2,y2,z2)) (z1',z2') | z1 > z1' && z2 < z2' = Just ((x1,y1),(x2,y2))
eliminateD _ _ _ = Nothing

-- | Only consider the given dimension of a cuboid
projectD :: Dimension -> Cuboid -> Segment
projectD X ((x1,_,_),(x2,_,_)) = (x1,x2)
projectD Y ((_,y1,_),(_,y2,_)) = (y1,y2)
projectD Z ((_,_,z1),(_,_,z2)) = (z1,z2)

count :: Foldable t => Int -> t a -> Bool
count n = (==n) . length

-- | The cuboid representation assumes that the points in each dimension are strictly increasing
--   We define cuboids by their bounds, not their "blocks".
type Cuboid = (Point, Point)
type Point = (Int, Int, Int)

-- Types used in dimension reduction analysis
type Rectangle = (Point2d, Point2d)
type Point2d = (Int, Int)
type Segment = (Point1d, Point1d)
type Point1d = Int
data Dimension = X | Y | Z deriving (Eq, Show)

d3s :: [Dimension]
d3s = [X,Y,Z]

-- | Is a Point inside a Cuboid
inside :: Cuboid -> Point -> Bool
inside ((x1,y1,z1),(x2,y2,z2)) (x,y,z) = x>=x1 && y>=y1 && z>=z1  &&  x<=x2 && y<=y2 && z<=z2

-- | Is a Point2d inside a Rectangle
inside2d :: Rectangle -> Point2d -> Bool
inside2d ((x1,y1),(x2,y2)) (x,y) = x>=x1 && y>=y1  &&  x<=x2 && y<=y2

-- | Enumerate all corners of a Cuboid
corners :: Cuboid -> [Point]
corners ((x1,y1,z1),(x2,y2,z2)) = [(x,y,z) | x<-[x1,x2], y<-[y1,y2], z<-[z1,z2]]

-- | Enumerate all corners of a Rectangle
corners2d :: Rectangle -> [Point2d]
corners2d ((x1,y1),(x2,y2)) = [(x,y) | x<-[x1,x2], y<-[y1,y2]]

-- | Test if a cuboid has any zero dimensions
zeroDimensional :: Cuboid -> Bool
zeroDimensional ((x1,y1,z1),(x2,y2,z2)) = x1 < x2 && y1 < y2 && z1 < z2

--- | splitCuboidByPoint has issues being mapped accross a list of points since it would not
--    converge on a set of cuboids in an order independent manner.
--    splitCuboidByPoints solves this issues by using a fold instead of a map.
--  | TODO: foldl'
splitCuboidByPoints :: [Point] -> Cuboid -> Set.Set Cuboid
splitCuboidByPoints ps c = foldl go (Set.singleton c) ps
    where
    go :: Set.Set Cuboid -> Point -> Set.Set Cuboid
    go cs p = Set.unions (Set.map (splitCuboidByPoint p) cs)

-- | For a point inside a cuboid, split it into 8 smaller cuboids
splitCuboidByPoint :: Point -> Cuboid -> Set.Set Cuboid
splitCuboidByPoint (x,y,z) ((x1,y1,z1),(x2,y2,z2)) = Set.fromList $ filter (not . zeroDimensional) -- Filter out empty cubes
    -- Should be 8 cases corresponding to each sub-cuboid
    [ ((x1,y1,z1),(x,y,z)) -- Origin corner
    , ((x,y,z),(x2,y2,z2)) -- Far corner

    , ((x,y1,z1),(x2,y,z)) -- Base Right
    , ((x1,y,z1),(x,y2,z)) -- Base Top
    , ((x1,y1,z),(x,y,z2)) -- Base Diagonal

    , ((x1,y,z),(x,y2,z2)) -- Apex Left
    , ((x,y1,z),(x2,y,z2)) -- Apex Bottom
    , ((x,y,z1),(x2,y2,z)) -- Apex Diagonal
    ]
    