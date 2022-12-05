{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NegativeLiterals #-}

module Advent23 where

{-

--- Day 23: Amphipod ---

A group of amphipods notice your fancy submarine and flag you down. "With such an impressive shell," one amphipod says, "surely you can help us with a question that has stumped our best scientists."

They go on to explain that a group of timid, stubborn amphipods live in a nearby burrow. Four types of amphipods live there: Amber (A), Bronze (B), Copper (C), and Desert (D). They live in a burrow that consists of a hallway and four side rooms. The side rooms are initially full of amphipods, and the hallway is initially empty.

They give you a diagram of the situation (your puzzle input), including locations of each amphipod (A, B, C, or D, each of which is occupying an otherwise open space), walls (#), and open space (.).

For example:

#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########

The amphipods would like a method to organize every amphipod into side rooms so that each side room contains one type of amphipod and the types are sorted A-D going left to right, like this:

#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #########

Amphipods can move up, down, left, or right so long as they are moving into an unoccupied open space. Each type of amphipod requires a different amount of energy to move one step: Amber amphipods require 1 energy per step, Bronze amphipods require 10 energy, Copper amphipods require 100, and Desert ones require 1000. The amphipods would like you to find a way to organize the amphipods that requires the least total energy.

However, because they are timid and stubborn, the amphipods have some extra rules:

Amphipods will never stop on the space immediately outside any room. They can move into that space so long as they immediately continue moving. (Specifically, this refers to the four open spaces in the hallway that are directly above an amphipod starting position.)

Amphipods will never move from the hallway into a room unless that room is their destination room and that room contains no amphipods which do not also have that room as their own destination. If an amphipod's starting room is not its destination room, it can stay in that room until it leaves the room. (For example, an Amber amphipod will not move from the hallway into the right three rooms, and will only move into the leftmost room if that room is empty or if it only contains other Amber amphipods.)

Once an amphipod stops moving in the hallway, it will stay in that spot until it can move into a room. (That is, once any amphipod starts moving, any other amphipods currently in the hallway are locked in place and will not move again until they can move fully into a room.)

In the above example, the amphipods can be organized using a minimum of 12521 energy. One way to do this is shown below.

Starting configuration:

#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########

One Bronze amphipod moves into the hallway, taking 4 steps and using 40 energy:

#############
#...B.......#
###B#C#.#D###
  #A#D#C#A#
  #########

The only Copper amphipod not in its side room moves there, taking 4 steps and using 400 energy:

#############
#...B.......#
###B#.#C#D###
  #A#D#C#A#
  #########

A Desert amphipod moves out of the way, taking 3 steps and using 3000 energy, and then the Bronze amphipod takes its place, taking 3 steps and using 30 energy:

#############
#.....D.....#
###B#.#C#D###
  #A#B#C#A#
  #########

The leftmost Bronze amphipod moves to its room using 40 energy:

#############
#.....D.....#
###.#B#C#D###
  #A#B#C#A#
  #########

Both amphipods in the rightmost room move into the hallway, using 2003 energy in total:

#############
#.....D.D.A.#
###.#B#C#.###
  #A#B#C#.#
  #########

Both Desert amphipods move into the rightmost room using 7000 energy:

#############
#.........A.#
###.#B#C#D###
  #A#B#C#D#
  #########

Finally, the last Amber amphipod moves into its room, using 8 energy:

#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #########

What is the least energy required to organize the amphipods?

-}

import Control.Monad (void, guard)
import Text.RawString.QQ (r)
import Algorithm.Search (aStar)
import Data.Map qualified as Map
import Data.Tuple (swap)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Debug.Trace (trace)
import Control.Arrow (Arrow(second))

-- | Solves day23
-- >>> day23 testInput
-- 14881
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
