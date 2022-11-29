{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveFunctor #-}

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

import Control.Monad (void)
import Text.RawString.QQ (r)
import Algorithm.Search (aStar)
import Data.Map qualified as Map

day23 :: String -> Integer
day23 i = maybe 0 fst $ aStar neighbours transitionCosts remainingCostEstimate solution players
    where

    -- Function to generate list of neighboring states given the current state
    neighbours :: Amphipods -> [Amphipods]
    neighbours as = filter valid $ map move $ chamberToList as
        where
        move :: (Location, Class) -> Amphipods
        move = undefined

        valid :: Amphipods -> Bool
        valid = undefined

    -- Function to generate transition costs between neighboring states.
    -- This is only called for adjacent states,
    -- so it is safe to have this function be partial for non-neighboring states.
    -- 
    -- TODO: This doesn't take into consideration the non-pausing in doorways rule.
    -- TODO: One way to do this - If you're loitering, move!
    transitionCosts :: Amphipods -> Amphipods -> Integer
    transitionCosts = classMovedCost

    -- Estimate on remaining cost given a state.
    -- Should never underestimate the cost.
    -- This is a gross overestimate, but should at least prove useful
    -- TODO: Should factor in distances from goals
    remainingCostEstimate :: Amphipods -> Integer
    remainingCostEstimate = product . map (classEstimate . snd) . filter (not.correct) . chamberToList
        where
        correct (k,v) = lookupChamber k solutionAmphipods == Just v
        classEstimate = (* boardSize) . classCost

    -- Predicate to determine if solution found.
    -- aStar returns the shortest path to the first state for which this predicate returns True
    solution :: Amphipods -> Bool
    solution = (== solutionAmphipods)

    -- Helper data
    parsed    = parseInput i
    players   = amphipods parsed
    board     = squares parsed
    boardSize = fromIntegral (Map.size (unChamber board))

-- | Determine which class of amphipod moved.
--   There may be a more efficient way to do this, but this is ok for now.
--   Should only ever be one item moved.
classMovedCost :: Amphipods -> Amphipods -> Integer
classMovedCost (Chamber a) (Chamber b) = classCost $ head $ Map.elems $ Map.difference a b

-- | Transforms chamber into squares
-- >>> squares (parseInput testInput)
-- Chamber {unChamber = fromList [((1,1),()),((2,1),()),((3,1),()),((3,2),()),((3,3),()),((4,1),()),((5,1),()),((5,2),()),((5,3),()),((6,1),()),((7,1),()),((7,2),()),((7,3),()),((8,1),()),((9,1),()),((9,2),()),((9,3),()),((10,1),()),((11,1),())]}
squares :: Chamber Char -> Squares
squares = void

-- | Transforms chamber into amphipods
-- >>> amphipods (parseInput testInput)
-- Chamber {unChamber = fromList [((3,2),'B'),((3,3),'A'),((5,2),'C'),((5,3),'D'),((7,2),'B'),((7,3),'C'),((9,2),'D'),((9,3),'A')]}
amphipods :: Chamber Char -> Amphipods
amphipods = mapChamber $ Map.filter (`elem` ['A'..'Z'])

-- | Parses Input...
-- >>> parseInput testInput
-- Chamber {unChamber = fromList [((1,1),'.'),((2,1),'.'),((3,1),'.'),((3,2),'B'),((3,3),'A'),((4,1),'.'),((5,1),'.'),((5,2),'C'),((5,3),'D'),((6,1),'.'),((7,1),'.'),((7,2),'B'),((7,3),'C'),((8,1),'.'),((9,1),'.'),((9,2),'D'),((9,3),'A'),((10,1),'.'),((11,1),'.')]}
parseInput :: String -> Chamber Char
parseInput i = Chamber $ Map.filter (`notElem` "# ") $ Map.fromList $
    zip [0..] (map (zip [0..]) (lines i))
    >>= (\(y,l) -> map (\(x,c) -> ((x,y),c)) l)

testInput :: String
testInput = unlines $ tail $ lines [r|
#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########
|]

solutionAmphipods :: Amphipods
solutionAmphipods = amphipods (parseInput solutionInput)

solutionInput :: String
solutionInput = unlines $ tail $ lines [r|
#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #########
|]

-- Chamber Helpers

lookupChamber :: Location -> Chamber a -> Maybe a
lookupChamber k = Map.lookup k . unChamber

chamberToList :: Chamber a -> [(Location, a)]
chamberToList = Map.toList . unChamber

mapChamberWithKey :: ((Location, a) -> (Location, b)) -> Chamber a -> Chamber b
mapChamberWithKey f = mapChamber $ Map.fromList . map f . Map.toList

mapChamber :: (Map.Map Location a -> Map.Map Location b) -> Chamber a -> Chamber b
mapChamber f = Chamber . f . unChamber

classCost :: Class -> Integer
classCost 'A' = 1
classCost 'B' = 10
classCost 'C' = 100
classCost 'D' = 1000
classCost _   = -1

-- Data Types

newtype Chamber x = Chamber { unChamber :: Map.Map Location x } deriving (Eq, Ord, Show, Functor)
type    Squares   = Chamber ()
type    Amphipods = Chamber Class -- Goblins
type    Location  = (Int,Int)
type    Class     = Char
