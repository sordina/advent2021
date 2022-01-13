{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DerivingStrategies #-}

module Advent20 where


{-

--- Day 20: Trench Map ---

With the scanners fully deployed, you turn their attention to mapping the floor of the ocean trench.

When you get back the image from the scanners, it seems to just be random noise. Perhaps you can combine an image enhancement algorithm and the input image (your puzzle input) to clean it up a little.

For example:

..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###

The first section is the image enhancement algorithm. It is normally given on a single line, but it has been wrapped to multiple lines in this example for legibility. The second section is the input image, a two-dimensional grid of light pixels (#) and dark pixels (.).

The image enhancement algorithm describes how to enhance an image by simultaneously converting all pixels in the input image into an output image. Each pixel of the output image is determined by looking at a 3x3 square of pixels centered on the corresponding input image pixel. So, to determine the value of the pixel at (5,10) in the output image, nine pixels from the input image need to be considered: (4,9), (4,10), (4,11), (5,9), (5,10), (5,11), (6,9), (6,10), and (6,11). These nine input pixels are combined into a single binary number that is used as an index in the image enhancement algorithm string.

For example, to determine the output pixel that corresponds to the very middle pixel of the input image, the nine pixels marked by [...] would need to be considered:

# . . # .
#[. . .].
#[# . .]#
.[. # .].
. . # # #

Starting from the top-left and reading across each row, these pixels are ..., then #.., then .#.; combining these forms ...#...#.. By turning dark pixels (.) into 0 and light pixels (#) into 1, the binary number 000100010 can be formed, which is 34 in decimal.

The image enhancement algorithm string is exactly 512 characters long, enough to match every possible 9-bit binary number. The first few characters of the string (numbered starting from zero) are as follows:

0         10        20        30  34    40        50        60        70
|         |         |         |   |     |         |         |         |
..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##

In the middle of this first group of characters, the character at index 34 can be found: #. So, the output pixel in the center of the output image should be #, a light pixel.

This process can then be repeated to calculate every pixel of the output image.

Through advances in imaging technology, the images being operated on here are infinite in size. Every pixel of the infinite output image needs to be calculated exactly based on the relevant pixels of the input image. The small input image you have is only a small region of the actual infinite input image; the rest of the input image consists of dark pixels (.). For the purposes of the example, to save on space, only a portion of the infinite-sized input and output images will be shown.

The starting input image, therefore, looks something like this, with more dark pixels (.) extending forever in every direction not shown here:

...............
...............
...............
...............
...............
.....#..#......
.....#.........
.....##..#.....
.......#.......
.......###.....
...............
...............
...............
...............
...............

By applying the image enhancement algorithm to every pixel simultaneously, the following output image can be obtained:

...............
...............
...............
...............
.....##.##.....
....#..#.#.....
....##.#..#....
....####..#....
.....#..##.....
......##..#....
.......#.#.....
...............
...............
...............
...............

Through further advances in imaging technology, the above output image can also be used as an input image! This allows it to be enhanced a second time:

...............
...............
...............
..........#....
....#..#.#.....
...#.#...###...
...#...##.#....
...#.....#.#...
....#.#####....
.....#.#####...
......##.##....
.......###.....
...............
...............
...............

Truly incredible - now the small details are really starting to come through. After enhancing the original input image twice, 35 pixels are lit.

Start with the original input image and apply the image enhancement algorithm twice, being careful to account for the infinite size of the images. How many pixels are lit in the resulting image?

--- Part Two ---

You still can't quite make out the details in the image. Maybe you just didn't enhance it enough.

If you enhance the starting input image in the above example a total of 50 times, 3351 pixels are lit in the final output image.

Start again with the original input image and apply the image enhancement algorithm 50 times. How many pixels are lit in the resulting image?

-}

import Text.RawString.QQ (r)
import Data.Array qualified as A
import qualified Data.Map as M
import Control.Arrow ((&&&), Arrow ((***)))
import Utils (parseGrid')
import Data.Maybe (fromMaybe)
import Data.Bool (bool)
import Data.List (findIndex, elemIndex)
import Numeric (readInt)
import GHC.Generics (Generic)
import Data.Generics.Product ()
import Control.Lens ((%~), (&), (.~), (^~), (^.))
import Data.Generics.Labels ()

data MapWithBoundary k v = MapB
  { boundary :: (k,k)
  , outside  :: v
  , explicit :: M.Map k v
  }
  deriving Show
  deriving Generic

resize :: (k->k) -> (k->k) -> MapWithBoundary k v -> MapWithBoundary k v
resize l h m = m & #boundary %~ (l***h)

grow2d :: (Ord k, Enum k) => MapWithBoundary (k,k) v -> MapWithBoundary (k,k) v
grow2d = resize (pred***pred) (succ***succ)

setOutside :: Bool -> MapWithBoundary k Bool -> MapWithBoundary k Bool
setOutside b m = m & #outside .~ b

around :: (Enum k, Ord k) => (k,k) -> MapWithBoundary (k,k) v -> [v]
around (x,y) m = [lookupB m (x', y') | y' <- [pred y .. succ y], x' <- [pred x .. succ x]]

lookupB :: (Ord k) => MapWithBoundary k v -> k -> v
lookupB m k = fromMaybe (outside m) (M.lookup k (explicit m))

mapFromKeys :: (Ord k, Enum k) => ((k,k) -> v) -> MapWithBoundary (k,k) v -> MapWithBoundary (k,k) v 
mapFromKeys f m = MapB b (outside m) $ M.fromList [((x,y), f (x,y)) |y<-[yl..yh], x<-[xl..xh]]
  where
  b@((xl,yl),(xh,yh)) = boundary m'
  m' = grow2d m

explore :: Enum k => MapWithBoundary (k,k) v -> ((k,k),(k,k))
explore = (!!10) . iterate ((pred***pred) *** (succ***succ)) . boundary

type World = MapWithBoundary (Int,Int) Bool

-- | Testing day20b
-- >>> day20b testInput
-- 3351

day20b :: String -> Int
day20b = length . lights . snd . (!!50) . iterate enhance . parseInput

-- | Testing day20
-- >>> day20 testInput
-- 35

day20 :: String -> Int
day20 = length . lights . snd . (!!2) . iterate enhance . parseInput

lights :: World -> [Bool]
lights m | outside m = error "Infinite lights"
         | otherwise = filter id $ M.elems $ explicit m

enhance :: (A.Array Int Bool, World) -> (A.Array Int Bool, World)
enhance (a,i) = (a, setOutside (enhanceAt a i (snd $ explore i)) $ mapFromKeys (enhanceAt a i) i)

enhanceAt :: A.Array Int Bool -> World -> (Int, Int) -> Bool
enhanceAt a i k = a A.! bs2ds (around k i)

-- >>> map (bool 'f' 't') (map (== '#') "...#...#.")
-- "ffftffftf"
-- >>> map (indexOf "ft") $ map (bool 'f' 't') (map (== '#') "...#...#.")
-- [0,0,0,1,0,0,0,1,0]
-- >>> bs2ds (map (== '#') "...#...#.")
-- 34


bs2ds :: [Bool] -> Int
bs2ds = fst . head . readInt 2 (`elem` "ft") (indexOf "ft") . map (bool 'f' 't')

indexOf :: String -> Char -> Int
indexOf s c = fromMaybe (error "indexOf couldn't find index") $ elemIndex c s

-- | Testing parseInput
-- >>> parseInput testInputSmall
-- (array (0,17) [(0,False),(1,False),(2,True),(3,False),(4,True),(5,False),(6,False),(7,True),(8,True),(9,True),(10,True),(11,True),(12,False),(13,True),(14,False),(15,True),(16,False),(17,True)],MapB {boundary = ((0,0),(4,0)), outside = False, explicit = fromList [((0,0),True),((1,0),False),((2,0),False),((3,0),True),((4,0),False)]})

parseInput :: String -> (A.Array Int Bool, World)
parseInput = (array . map (== '#') . head &&& image . tail . tail) . lines

array :: [e] -> A.Array Int e
array xs = A.listArray (0, pred (length xs)) xs

image :: [String] -> World
image s = MapB ((xl,yl),(xh,yh)) False m
  where 
  m       = parseGrid' (== '#') $ unlines s
  ks      = M.keys m
  (xl,xh) = (minimum &&& maximum) $ map fst ks
  (yl,yh) = (minimum &&& maximum) $ map snd ks

testInput :: String
testInput = unlines $ tail $ lines [r|
..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###
|]

testInputSmall :: String
testInputSmall = unlines $ tail $ lines [r|
..#.#..#####.#.#.#

#..#.
|]
