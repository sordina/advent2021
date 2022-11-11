{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}

module Advent22b where

{-

--- Day 22b: Reactor Reboot - A second approach to solving the 22b unbounded cube problem ---

-}

import Text.RawString.QQ (r)
import Data.List.Split (splitOn)
import Control.Arrow ((&&&))
import Data.Monoid (Last (Last, getLast), Sum (getSum))
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (foldl')
import Data.Foldable (fold)
import Data.Bool (bool)

import Data.Vector qualified as V
import Data.Set qualified as S

-- | Testing day22b
-- >>> foo
-- [(1,2)]

data R = R Int Int deriving (Eq, Ord, Show)
type E = V.Vector R
type SE = V.Vector (S.Set R)

-- | Cases:
-- 
-- 1. a----b  A
--            c-----d B
-- 
-- 2. a----b  A
--       c-----d B
-- 
-- 3.    a----b  A
--    c-----d B
-- 
-- 4.          a----b  A
--    c-----d B
-- 
-- 5. a----b  A
--     c-d B
-- 
-- 6.   a-b A
--    c-----d B
-- 
engage :: R -> R -> Maybe R
engage (R a b) (R c d)
    | {- 1. -} b < c = Nothing
    | {- 2. -} b <= d && a <= c = Just (R c b)
    | {- 3. -} c <= a && d <= b = Just (R a d)
    | {- 4. -} d <= a = Nothing
    | {- 5. -} a <= c && d <= b = Just (R c d)
    | {- 6. -} c <= a && b <= d = Just (R a b)
    | otherwise = Nothing



