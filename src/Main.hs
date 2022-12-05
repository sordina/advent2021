module Main where

import           Data.List          (intercalate)
import           Data.Maybe         (fromMaybe)
import           System.Environment (getArgs)

import qualified Advent01
import qualified Advent01b
import qualified Advent02
import qualified Advent03
import qualified Advent04
import qualified Advent05
import qualified Advent06
import qualified Advent07
import qualified Advent08
import qualified Advent09
import qualified Advent10
import qualified Advent11
import qualified Advent12
import qualified Advent13
import qualified Advent14
import qualified Advent15
import qualified Advent16
import qualified Advent17
import qualified Advent18
import qualified Advent19
import qualified Advent20
import qualified Advent21
import qualified Advent21b
import qualified Advent22
import qualified Advent22b
import qualified Advent23

interactShow :: Show a => (String -> a) -> IO ()
interactShow f = interact ((++ "\n") . show . f)

days :: [(String, IO ())]
days =
    [ ("01",  interactShow Advent01.day1)
    , ("01b", interactShow Advent01b.day1b)
    , ("02",  interactShow Advent02.day2)
    , ("02b", interactShow Advent02.day2b)
    , ("03",  interactShow Advent03.day3)
    , ("03b", interactShow (uncurry (*) . Advent03.day3b))
    , ("04",  interactShow Advent04.day4)
    , ("04b", interactShow Advent04.day4b)
    , ("05",  interactShow Advent05.day5)
    , ("05b", interactShow Advent05.day5b)
    , ("06",  interactShow Advent06.day6)
    , ("06b", interactShow Advent06.day6b)
    , ("07",  interactShow Advent07.day7)
    , ("07b", interactShow Advent07.day7b)
    , ("08",  interactShow Advent08.day8)
    , ("08b", interactShow Advent08.day8b)
    , ("09",  interactShow Advent09.day9)
    , ("09b", interactShow Advent09.day9b) -- Repeat again to allow leading 0s
    , ("1",  interactShow Advent01.day1)
    , ("1b", interactShow Advent01b.day1b)
    , ("2",  interactShow Advent02.day2)
    , ("2b", interactShow Advent02.day2b)
    , ("3",  interactShow Advent03.day3)
    , ("3b", interactShow (uncurry (*) . Advent03.day3b))
    , ("4",  interactShow Advent04.day4)
    , ("4b", interactShow Advent04.day4b)
    , ("5",  interactShow Advent05.day5)
    , ("5b", interactShow Advent05.day5b)
    , ("6",  interactShow Advent06.day6)
    , ("6b", interactShow Advent06.day6b)
    , ("7",  interactShow Advent07.day7)
    , ("7b", interactShow Advent07.day7b)
    , ("8",  interactShow Advent08.day8)
    , ("8b", interactShow Advent08.day8b)
    , ("9",  interactShow Advent09.day9)
    , ("9b", interactShow Advent09.day9b)
    , ("10", interactShow Advent10.day10)
    , ("10b",interactShow Advent10.day10b)
    , ("11", interactShow Advent11.day11)
    , ("11b",interactShow Advent11.day11b)
    , ("12", interactShow Advent12.day12)
    , ("12b",interactShow Advent12.day12b)
    , ("13", interactShow Advent13.day13)
    , ("13b",interact     Advent13.day13b)
    , ("14", interactShow Advent14.day14)
    , ("14b",interactShow Advent14.day14b)
    , ("15", interactShow Advent15.day15)
    , ("15b",interactShow Advent15.day15b)
    , ("16", interactShow Advent16.day16)
    , ("16b",interactShow Advent16.day16b)
    , ("17", interactShow Advent17.day17)
    , ("17b",interactShow Advent17.day17b)
    , ("18", interactShow Advent18.day18)
    , ("18b",interactShow Advent18.day18b)
    , ("19", interactShow Advent19.day19)
    , ("19b",interactShow Advent19.day19b)
    , ("20", interactShow Advent20.day20)
    , ("20b",interactShow Advent20.day20b)
    , ("21", interactShow Advent21.day21)
    , ("21b",interactShow Advent21b.day21b)
    , ("22", interactShow Advent22.day22)
    , ("22b",interactShow Advent22b.day22b)
    , ("23", interactShow Advent23.day23)
    ]

help :: a
help = error $ "Usage: advent2021 (" ++ intercalate " | " (map fst days) ++ ")"

main :: IO ()
main = do
    as <- getArgs
    case as of
        [d] -> fromMaybe help (lookup d days)
        _   -> help
