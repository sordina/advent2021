{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Advent21b where

import Text.RawString.QQ (r)
import Control.Arrow ((&&&))
import Control.Lens (_1,_2, (^.), (&), (.~), view)
import Debug.Trace (traceShow)
import Data.Tree ( Tree(Node), unfoldForest, drawForest )
import Data.Foldable (foldl')
import Data.List (partition)
import Data.Tuple (swap)
import Data.Function (fix)
import Data.MemoCombinators ( integral, pair, Memo )

-- | Testing day21x
-- >>> day21b testInput
-- 444356092776315

-- | Testing parseInput
-- >>> parseInput testInput
-- ((0,4),(0,8))

-- | Confirming non-memoized results
-- >>> start 0 $ parseInput testInput
-- (27,0)
-- >>> start 1 $ parseInput testInput
-- (27,0)
-- >>> start 2 $ parseInput testInput
-- (183,156)
-- >>> start 3 $ parseInput testInput
-- (990,207)

testInput :: String
testInput = unlines $ tail $ lines [r|
Player 1 starting position: 4
Player 2 starting position: 8
|]

day21b :: String -> Int
day21b = uncurry max . start threshold . parseInput

type Player    = (Int    {- Score     -}, Int    {- Position  -})
type Game      = (Player {- P1 Player -}, Player {- P2 Player -})
type Wins      = (Int    {- P1 Wins   -}, Int    {- P2 Wins   -})
type Threshold = Int

parseInput :: String -> Game
parseInput i = (parseLine "1" (head ls), parseLine "2" (ls !! 1))
  where
  ls = map words $ lines i
  parseLine n (a:b:c:d:e:_) | n == b = (0, read e)
  parseLine _ ws = error $ "can't parse line: " <> unwords ws

start :: Threshold -> Game -> Wins
start t = fix (gamo . p1Roll t)

p1Roll :: Threshold -> (Game -> Wins) -> Game -> Wins
p1Roll t p1Roll g = immediateWins <<>> deferredWins
  where
    deferredWins = gconcat $ map (p2Roll p1Roll) r
    immediateWins = (length w, 0)
    (w, r) = partition ((>= t) . view (_1._1)) games
    games = do
      d1 <- roll
      d2 <- roll
      d3 <- roll
      let
        p = mod10 $ d1 + d2 + d3 + (g ^. _1 . _2)
        s = p + (g ^. _1 . _1)
      pure $ g & _1 .~ (s,p)

p2Roll :: (Game -> Wins) -> Game -> Wins
p2Roll p1Roll = swap . p1Roll . swap

-- Helpers

gconcat :: [Wins] -> Wins
gconcat = foldl' (<<>>) (0,0)

(<<>>) :: Wins -> Wins -> Wins
(w1,w2) <<>> (x1,x2) = (w1+x1, w2+x2)

gamo :: Memo Game
gamo = let p = pair integral integral in pair p p

-- Constants

threshold :: Threshold
threshold = 21

roll :: [Int]
roll = [1,2,3]

mod10 :: Integral a => a -> a
mod10 n = succ $ pred n `mod` 10
