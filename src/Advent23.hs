{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE QuasiQuotes #-}

module Advent23 where

import Control.Monad (void, guard)
import Text.RawString.QQ (r)
import Algorithm.Search (aStar)
import Data.Map qualified as Map
import Data.Tuple (swap)
import Data.Maybe (isJust, isNothing, fromMaybe, mapMaybe)
import Debug.Trace (trace)
import Utils (traceWithId)
import Control.Arrow (Arrow(second))
import Control.Lens (view, _1, _2, _3, _4, (%~), (&))

-- | Solves day23
-- >>> day23 testInput
-- 12681
day23 :: String -> Integer
day23 i = maybe -1 fst $ traceChambers $ aStar neighbours transitionCosts heuristic solution players
-- day23 i = maybe -1 fst $ traceChambers $ aStar neighbours transitionCosts remainingCostEstimate solution players
    where
    -- Function to generate list of neighboring states given the current state
    -- If anyone is loitering, only they can move
    neighbours :: State -> [State]
    neighbours as@(chamberToList . view _1 -> ls) = concatMap adjacentA if null loitering then ls else loitering
        where
        loitering = filter (isJust . flip lookupChamber doorways . fst) ls

        adjacentA :: (Location, (ID,Class)) -> [State]
        adjacentA (p,x@(id_,c)) = do
            let as' = as & _1 %~ deleteChamber p
                ch = view _1 as
            p' <- adjacentC p
            guard (isJust $ lookupChamber p' parsed) -- Is on the board
            guard (isNothing $ lookupChamber p' ch) -- Isn't stacking on top of another Amphipod
            guard (allowedToEnterChamber (anonymiseChamber ch) p p' c) -- Isn't barging in on guests or visiting
            guard (resumed as id_) -- Going back home!
            let next = updateState as' p p' x
            -- guard (not $ Set.member (anonymiseChamber $ view _1 as) (view _4 as))
            -- pure $ traceWithId (renderChamber . anonymiseChamber . view _1) next
            pure next

    resumed :: State -> ID -> Bool
    resumed (_,_,id') id'' = maybe True (id'' ==) id'

    heuristic :: State -> Integer
    heuristic (c,_,_)
        = product (map manhattan (chamberToList (anonymiseChamber c)))
        * choose (sizeChamber parsed) (fromIntegral $ Map.size $ unChamber (anonymiseChamber c) Map.\\ unChamber end)

    manhattan :: ((Integer,Integer), Class) -> Integer
    manhattan ((x,y),c) = (classCost c *) $ product $ map (\(x',y') -> (1+) $ abs (x-x') + abs (if x == x' then y-y' else 4 - abs (y-y'))) (lookupExits c)

    updateState :: State -> Location -> Location -> (ID,Class) -> State
    updateState (w,m,f) l l' x@(id_,_) = (insertChamber l' x w, Just id_, f')
        where
        f' = if hallway && notHome && (currentlyMoving || someoneElseMoving) then Just id_ else Nothing
        hallway           = isNothing (lookupChamber l end)
        notHome           = isNothing (lookupChamber l' end)
        currentlyMoving   = f == Just id_
        someoneElseMoving = isNothing f && maybe False (id_ /=) m

    -- Function to generate transition costs between neighboring states.
    -- This is only called for adjacent states,
    -- so it is safe to have this function be partial for non-neighboring states.
    -- 
    -- | Determine which class of amphipod moved.
    --   There may be a more efficient way to do this, but this is ok for now.
    --   Should only ever be one item moved.
    -- 
    -- Note: This assumes that only one Amphipod has moved only one position
    transitionCosts :: State -> State -> Integer
    transitionCosts (anonymised -> Chamber a) (anonymised -> Chamber b) = classCost $ head $ Map.elems $ Map.difference a b

    -- Predicate to determine if solution found.
    -- aStar returns the shortest path to the first state for which this predicate returns True
    solution :: State -> Bool
    solution = (== end) . anonymised

    anonymised = anonymiseChamber . view _1

    -- Helpers
    parsed = parseInput i

    players :: State
    players = (identifyChamber $ amphipods parsed, Nothing, Nothing)

allowedToEnterChamber :: Amphipods -> Location -> Location -> Class -> Bool
allowedToEnterChamber as l n c
    =  isNothing (lookupChamber n end) -- Next location isn't a chamber
    || isJust (lookupChamber l end) -- Already in a chamber
    || ( chamberEmpty as n c
      && lookupChamber n end == Just c -- Own chamber
       )

chamberEmpty :: Amphipods -> Location -> Class -> Bool
chamberEmpty as l c = maybe False (not . any (/= c) . mapMaybe (`lookupChamber` as) . lookupExits) (lookupChamber l end)

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

lookupExits :: Class -> [Location]
lookupExits c = fromMaybe [] $ Map.lookup c exits

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

choose :: Integral a => a -> a -> a
choose n k = fact n `div` (fact k * fact (n-k))

fact :: (Num a, Enum a) => a -> a
fact n = product [1..n]

traceChambers :: Maybe (a, [State]) -> Maybe (a, [State])
traceChambers Nothing = trace "--- \n" Nothing
traceChambers x@(Just (_,xs)) = trace (concatMap (renderChamber . anonymiseChamber . view _1) xs) x

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

identifyChamber :: Chamber a -> Chamber (Int,a)
identifyChamber = mapChamber (Map.fromList . zipWith (\i (k,v) -> (k,(i,v))) [1..] . Map.toList)

anonymiseChamber :: Chamber (Int, a) -> Chamber a
anonymiseChamber = fmap snd

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
type    Amphids   = Chamber (Int,Class) -- Goblins with IDs
type    Location  = (Integer, Integer)
type    Class     = Char
type    ID        = Int
-- type    State     = (Amphids, Maybe ID, Maybe ID, Set.Set Amphipods) -- who was moving, who must move
type    State     = (Amphids, Maybe ID, Maybe ID) -- who was moving, who must move
