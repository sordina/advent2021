module Main where

import           Control.Arrow      (Arrow ((&&&)))
import           Data.List          (intercalate)
import           Data.Maybe         (fromMaybe)
import           System.Environment (getArgs)

import qualified Advent01  (day1)
import qualified Advent01b (day1b)
import qualified Advent02  (day2)

interactShow :: Show a => (String -> a) -> IO ()
interactShow f = interact (show . f)

days :: [(String, IO ())]
days =
    [ ("1",  interactShow Advent01.day1)
    , ("1b", interactShow Advent01b.day1b)
    , ("2",  interactShow Advent02.day2)
    ]

help :: a
help = error $ "Usage: advent2021 (" ++ intercalate " | " (map fst days) ++ ")"

main :: IO ()
main = do
    as <- getArgs
    case as of
        [d] -> fromMaybe help (lookup d days)
        _   -> help
