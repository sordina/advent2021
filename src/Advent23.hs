{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NegativeLiterals #-}

module Advent23 where

import Control.Monad (void, guard)
import Text.RawString.QQ (r)
import Algorithm.Search (aStar, dijkstra)
import Data.Map qualified as Map
import Data.Tuple (swap)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Debug.Trace (trace)
import Control.Arrow (Arrow(second))

-- | Solves day23
-- >>> day23 testInput
-- 12681
day23 :: String -> Integer
day23 i = maybe -1 fst $ traceChambers $ aStar neighbours transitionCosts remainingCostEstimate solution players
    where
    -- Function to generate list of neighboring states given the current state
    -- If anyone is loitering, only they can move
    neighbours :: Amphipods -> [Amphipods]
    neighbours as = concatMap adjacentA if null loitering then ls else loitering
        where
        ls = chamberToList as
        loitering = filter (isJust . flip lookupChamber doorways . fst) ls

        adjacentA :: (Location, Class) -> [Amphipods]
        adjacentA (p,c) = do
            let as' = deleteChamber p as
            p' <- adjacentC p
            guard (isJust $ lookupChamber p' parsed) -- Is on the board
            guard (isNothing $ lookupChamber p' as) -- Isn't stacking on top of another Amphipod
            pure $ insertChamber p' c as'

    -- Function to generate transition costs between neighboring states.
    -- This is only called for adjacent states,
    -- so it is safe to have this function be partial for non-neighboring states.
    -- 
    -- | Determine which class of amphipod moved.
    --   There may be a more efficient way to do this, but this is ok for now.
    --   Should only ever be one item moved.
    -- 
    -- Note: This assumes that only one Amphipod has moved only one position
    transitionCosts :: Amphipods -> Amphipods -> Integer
    transitionCosts (Chamber a) (Chamber b) = classCost $ head $ Map.elems $ Map.difference a b

    -- Estimate on remaining cost given a state.
    -- Should never underestimate the cost.
    -- This is a gross overestimate, but should at least prove useful
    remainingCostEstimate :: Amphipods -> Integer
    remainingCostEstimate = product . map estimate . chamberToList
        where
        estimate (k,v) = classCost v * distance (k,v)
        distance (k,v) = succ $ maybe (error "oops") (sum . map (manhattan k)) (Map.lookup v exits)

    -- Predicate to determine if solution found.
    -- aStar returns the shortest path to the first state for which this predicate returns True
    solution :: Amphipods -> Bool
    solution = (== end)

    -- Helpers
    parsed  = parseInput i
    players = amphipods parsed

    manhattan (x,y) (x',y') =
        if x == x'
            then abs (y-y')
            else abs (x-x') + (y-1) + (y'-1)

-- | Transforms chamber into amphipods
-- >>> amphipods (parseInput testInput)
-- Chamber {unChamber = fromList [((3,2),'B'),((3,3),'A'),((5,2),'C'),((5,3),'D'),((7,2),'B'),((7,3),'C'),((9,2),'D'),((9,3),'A')]}
amphipods :: Chamber Char -> Amphipods
amphipods = mapChamber $ Map.filter (`elem` ['A'..'Z'])

prohibited :: Chamber Char -> Squares
prohibited = void . mapChamber (Map.filter (`elem` ","))

-- | Parses Input...
-- >>> parseInput testInput
-- Chamber {unChamber = fromList [((1,1),'.'),((2,1),'.'),((3,1),'.'),((3,2),'B'),((3,3),'A'),((4,1),'.'),((5,1),'.'),((5,2),'C'),((5,3),'D'),((6,1),'.'),((7,1),'.'),((7,2),'B'),((7,3),'C'),((8,1),'.'),((9,1),'.'),((9,2),'D'),((9,3),'A'),((10,1),'.'),((11,1),'.')]}
parseInput :: String -> Chamber Char
parseInput i = Chamber $ Map.filter (`notElem` "# ") $ Map.fromList $
    zip [0..] (map (zip [0..]) (lines i))
    >>= (\(y,l) -> map (\(x,c) -> ((x,y),c)) l)

solutions :: Chamber Char
solutions = parseInput solutionInput

doorways :: Squares
doorways = prohibited solutions

end :: Amphipods
end = amphipods solutions

exits :: Map.Map Char [Location]
exits = Map.fromListWith (<>) $ map (second pure . swap) $  Map.toList $ unChamber solutions

testInput :: String
testInput = unlines $ tail $ lines [r|
#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########
|]

solutionInput :: String
solutionInput = unlines $ tail $ lines [r|
#############
#..,.,.,.,..#
###A#B#C#D###
  #A#B#C#D#
  #########
|]

-- Chamber Helpers

traceChambers :: Maybe (a, [Amphipods]) -> Maybe (a, [Amphipods])
traceChambers Nothing = trace "--- \n" Nothing
traceChambers x@(Just (_,xs)) = trace (concatMap renderChamber xs) x

renderChamber :: Amphipods -> String
renderChamber as = unlines $
    flip map [0..h] \y -> do
        flip map [0..w] \x -> do
            fromMaybe
                (fromMaybe ' ' (lookupChamber (x,y) (fmap o c)))
                (lookupChamber (x, y) as)
    where
    o = \x -> if x `elem` ['A'..'Z'] then ' ' else x
    c = parseInput testInput
    l = lines testInput
    h = fromIntegral $ length l
    w = fromIntegral $ length $ head l

lookupChamber :: Location -> Chamber a -> Maybe a
lookupChamber k = Map.lookup k . unChamber

deleteChamber :: Location -> Chamber a -> Chamber a
deleteChamber k = mapChamber $ Map.delete k

insertChamber :: Location -> a -> Chamber a -> Chamber a
insertChamber k v = mapChamber $ Map.insert k v

sizeChamber :: Chamber a -> Integer
sizeChamber = fromIntegral . Map.size . unChamber

chamberToList :: Chamber a -> [(Location, a)]
chamberToList = Map.toList . unChamber

mapChamberWithKey :: ((Location, a) -> (Location, b)) -> Chamber a -> Chamber b
mapChamberWithKey f = mapChamber $ Map.fromList . map f . Map.toList

mapChamber :: (Map.Map Location a -> Map.Map Location b) -> Chamber a -> Chamber b
mapChamber f = Chamber . f . unChamber

adjacentC :: (Enum a, Enum b, Eq a, Eq b) => (a, b) -> [(a, b)]
adjacentC (x,y) = [(pred x, y), (succ x, y), (x, pred y), (x, succ y)]

classCost :: Class -> Integer
classCost 'A' = 1
classCost 'B' = 10
classCost 'C' = 100
classCost 'D' = 1000
classCost _   = 1000000000

-- Data Types

newtype Chamber x = Chamber { unChamber :: Map.Map Location x } deriving (Eq, Ord, Show, Functor)
type    Squares   = Chamber ()
type    Amphipods = Chamber Class -- Goblins
type    Location  = (Integer, Integer)
type    Class     = Char
