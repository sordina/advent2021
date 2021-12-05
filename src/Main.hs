module Main where

import           Control.Arrow      (Arrow ((&&&)))
import           Data.List          (intercalate)
import           Data.Maybe         (fromMaybe)
import           System.Environment (getArgs)

import qualified Advent01  (day1)
import qualified Advent01b (day1b)
import qualified Advent02  (day2, day2b)
import qualified Advent03  (day3, day3b)
import qualified Advent04  (day4, day4b)
import qualified Advent05  (day5, day5b)

interactShow :: Show a => (String -> a) -> IO ()
interactShow f = interact (show . f)

days :: [(String, IO ())]
days =
    [ ("1",  interactShow Advent01.day1)
    , ("1b", interactShow Advent01b.day1b)
    , ("2",  interactShow Advent02.day2)
    , ("2b", interactShow Advent02.day2b)
    , ("3",  interactShow Advent03.day3)
    , ("3b", interactShow (uncurry (*) . Advent03.day3b))
    , ("4",  interactShow Advent04.day4)
    , ("4b", interactShow Advent04.day4b)
    , ("5",  interactShow Advent05.day5)
    , ("5b", interactShow Advent05.day5b)
    ]

help :: a
help = error $ "Usage: advent2021 (" ++ intercalate " | " (map fst days) ++ ")"

main :: IO ()
main = do
    as <- getArgs
    case as of
        [d] -> fromMaybe help (lookup d days)
        _   -> help
