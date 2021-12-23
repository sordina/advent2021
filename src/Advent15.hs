{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NegativeLiterals #-}

module Advent15 where

{-

--- Day 15: Chiton ---

You've almost reached the exit of the cave, but the walls are getting closer together. Your submarine can barely still fit, though; the main problem is that the walls of the cave are covered in chitons, and it would be best not to bump any of them.

The cavern is large, but has a very low ceiling, restricting your motion to two dimensions. The shape of the cavern resembles a square; a quick scan of chiton density produces a map of risk level throughout the cave (your puzzle input). For example:

1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581

You start in the top left position, your destination is the bottom right position, and you cannot move diagonally. The number at each position is its risk level; to determine the total risk of an entire path, add up the risk levels of each position you enter (that is, don't count the risk level of your starting position unless you enter it; leaving it adds no risk to your total).

Your goal is to find a path with the lowest total risk. In this example, a path with the lowest total risk is highlighted here:

1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581

The total risk of this path is 40 (the starting position is never entered, so its risk is not counted).

What is the lowest total risk of any path from the top left to the bottom right?

--- Part Two ---

Now that you know how to find low-risk paths in the cave, you can try to find your way out.

The entire cave is actually five times larger in both dimensions than you thought; the area you originally scanned is just one tile in a 5x5 tile area that forms the full map. Your original map tile repeats to the right and downward; each time the tile repeats to the right or downward, all of its risk levels are 1 higher than the tile immediately up or left of it. However, risk levels above 9 wrap back around to 1. So, if your original map had some position with a risk level of 8, then that same position on each of the 25 total tiles would be as follows:

8 9 1 2 3
9 1 2 3 4
1 2 3 4 5
2 3 4 5 6
3 4 5 6 7

Each single digit above corresponds to the example position with a value of 8 on the top-left tile. Because the full map is actually five times larger in both dimensions, that position appears a total of 25 times, once in each duplicated tile, with the values shown above.

Here is the full five-times-as-large version of the first example above, with the original map in the top left corner highlighted:

11637517422274862853338597396444961841755517295286
13813736722492484783351359589446246169155735727126
21365113283247622439435873354154698446526571955763
36949315694715142671582625378269373648937148475914
74634171118574528222968563933317967414442817852555
13191281372421239248353234135946434524615754563572
13599124212461123532357223464346833457545794456865
31254216394236532741534764385264587549637569865174
12931385212314249632342535174345364628545647573965
23119445813422155692453326671356443778246755488935
22748628533385973964449618417555172952866628316397
24924847833513595894462461691557357271266846838237
32476224394358733541546984465265719557637682166874
47151426715826253782693736489371484759148259586125
85745282229685639333179674144428178525553928963666
24212392483532341359464345246157545635726865674683
24611235323572234643468334575457944568656815567976
42365327415347643852645875496375698651748671976285
23142496323425351743453646285456475739656758684176
34221556924533266713564437782467554889357866599146
33859739644496184175551729528666283163977739427418
35135958944624616915573572712668468382377957949348
43587335415469844652657195576376821668748793277985
58262537826937364893714847591482595861259361697236
96856393331796741444281785255539289636664139174777
35323413594643452461575456357268656746837976785794
35722346434683345754579445686568155679767926678187
53476438526458754963756986517486719762859782187396
34253517434536462854564757396567586841767869795287
45332667135644377824675548893578665991468977611257
44961841755517295286662831639777394274188841538529
46246169155735727126684683823779579493488168151459
54698446526571955763768216687487932779859814388196
69373648937148475914825958612593616972361472718347
17967414442817852555392896366641391747775241285888
46434524615754563572686567468379767857948187896815
46833457545794456865681556797679266781878137789298
64587549637569865174867197628597821873961893298417
45364628545647573965675868417678697952878971816398
56443778246755488935786659914689776112579188722368
55172952866628316397773942741888415385299952649631
57357271266846838237795794934881681514599279262561
65719557637682166874879327798598143881961925499217
71484759148259586125936169723614727183472583829458
28178525553928963666413917477752412858886352396999
57545635726865674683797678579481878968159298917926
57944568656815567976792667818781377892989248891319
75698651748671976285978218739618932984172914319528
56475739656758684176786979528789718163989182927419
67554889357866599146897761125791887223681299833479

Equipped with the full map, you can now find a path from the top left corner to the bottom right corner with the lowest total risk:

...

The total risk of this path is 315 (the starting position is still never entered, so its risk is not counted).

Using the full map, what is the lowest total risk of any path from the top left to the bottom right?

-}

import Text.RawString.QQ (r)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes, mapMaybe, isJust)
import Data.Ord (comparing)
import Data.Foldable (minimumBy)
import Utils
import Control.Arrow ((&&&), Arrow ((***), second, first))
import Data.Function (on)
import qualified Data.Set as Set
import Algorithm.Search (aStar)

-- | Testing day15b
-- >>> day15b testInput
-- WAS WAS 315
-- WAS NOW 532
-- NOW 483

-- Path: (315,[(49,49),(48,49),(47,49),(46,49),(45,49),(45,48),(45,47),(44,47),(43,47),(42,47),(42,46),(41,46),(41,45),(41,44),(41,43),(40,43),(39,43),(38,43),(37,43),(37,42),(37,41),(37,40),(37,39),(36,39),(35,39),(34,39),(34,38),(34,37),(33,37),(33,36),(32,36),(32,35),(32,34),(31,34),(30,34),(29,34),(29,33),(28,33),(27,33),(27,32),(27,31),(27,30),(26,30),(25,30),(24,30),(24,29),(23,29),(22,29),(22,28),(21,28),(20,28),(19,28),(19,27),(19,26),(19,25),(18,25),(17,25),(16,25),(16,24),(16,23),(16,22),(15,22),(15,21),(14,21),(14,20),(14,19),(13,19),(12,19),(12,18),(11,18),(10,18),(9,18),(9,17),(9,16),(8,16),(7,16),(6,16),(5,16),(4,16),(3,16),(3,15),(2,15),(2,14),(2,13),(2,12),(1,12),(0,12),(0,11),(0,10),(0,9),(0,8),(0,7),(0,6),(0,5),(0,4),(0,3),(0,2),(0,1),(0,0)])

day15b :: String -> Int
day15b = withScore solve' . embiggen . parseInput

-- >>> map in_1_9 [1..15]
-- [1,2,3,4,5,6,7,8,9,1,2,3,4,5,6]

in_1_9 :: Int -> Int
in_1_9 n = succ $ pred n `mod` 9

-- >>> Map.findMax $ embiggen $ Map.fromList [((0,0),8), ((0,1),4)]
-- ((4,9),3)

embiggen :: Map.Map (Int,Int) Int -> Map.Map (Int,Int) Int
embiggen m = Map.map in_1_9 $ Map.unions $ concat m'''
  where
  (w,h)  = (succ *** succ) $ fst $ Map.findMax m
  m'''   =      zipWith addy [0..]  m''
  m''    = map (zipWith addx [0..]) m'
  m'     = replicate 5 $ replicate 5 m

  addy :: Int -> [Map.Map (Int,Int) Int] -> [Map.Map (Int,Int) Int]
  addy n = map (Map.mapKeys (second (\ky -> ky + h * n)) . Map.map (+n))

  addx :: Int -> Map.Map (Int,Int) Int -> Map.Map (Int,Int) Int
  addx n = Map.mapKeys (first (\kx -> kx + w * n)) . Map.map (+n)

-- >>> embiggen (parseInput testInput) == parseInput testInput5
-- True

-- | Testing day15
-- >>> day15 testInput
-- 40

day15 :: String -> Int
day15 = withScore solve' . parseInput

-- >>> solve' (parseInput testInputBT)
-- [(4,2),(4,1),(3,1),(2,1),(2,2),(1,2),(0,2),(0,1),(0,0)]

-- >>> withScore solve' (parseInput testInput)
-- 40

-- >>> withScore solve' (embiggen $ parseInput testInput)
-- 315

withScore :: (Num a, Ord k) => (Map.Map k a -> [k]) -> Map.Map k a -> a
withScore s m = sum $ mapMaybe (`Map.lookup` m) (s m)

{-
19999
19111
11191
-}

solve' :: Map.Map (Int, Int) Int -> [(Int, Int)]
solve' m = fromMaybe (error "oops!") (findPath (0,0) edge)
  where
  (edge, _) = Map.findMax m

  adjacent :: (Int, Int) -> [(Int, Int)]
  adjacent (x, y) = filter (isJust . flip Map.lookup m) [sx,sy,sa,sb]
    where
    sx = (pred x, y)
    sy = (x, pred y)
    sa = (succ x, y)
    sb = (x, succ y)

  costFn :: (Int, Int) -> (Int, Int) -> Int
  costFn p1@(x1,y1) p2@(x2,y2) = sum $ mapMaybe (`Map.lookup` m) $ pointsBetween p1 p2

  findPath :: (Int, Int) -> (Int, Int) -> Maybe [(Int, Int)]
  findPath start end = snd <$> aStar next cost remaining (== end) start
    where
    next = adjacent
    cost = costFn
    remaining p = uncurry (+) $ edge -~ p

pointsBetween :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
pointsBetween (ax,ay) (bx,by) = map (p1 +~) $ crossProductZero (p2 -~ p1)
  where
    p1 = (min ax bx, min ay by)
    p2 = (max ax bx, max ay by)

(+~) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(a,b) +~ (c,d) = (a+c,b+d)

(-~) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
p1 -~ (c,d) = p1 +~ (negate c, negate d)


-- >>> compare 1 2
-- LT

-- >>> miniScore 0 0 [1,2] [9, undefined]
-- LT
-- >>> miniScore 0 0 [9, undefined] [1,2]
-- GT
-- >>> miniScore 0 0 [9] [9]
-- EQ

miniScore :: (Ord a, Num a) => a -> a -> [a] -> [a] -> Ordering
miniScore a b [] []                           = compare a b
miniScore a b [] (y:ys)           | b > a     = LT
                                  | otherwise = miniScore a (b+y) [] ys
miniScore a b (x:xs) []           | a > b     = GT
                                  | otherwise = miniScore (a+x) b xs []
miniScore a b x'@(x:xs) y'@(y:ys) | a > b     = miniScore a (b+y) x' ys
                                  | otherwise = miniScore (a+x) b xs y'

parseInput :: String -> Map.Map (Int,Int) Int
parseInput s = Map.fromList cs
  where
  ls = lines s
  cs = concat $ zipWith (\y l -> zipWith (\ x c -> ((x, y), read [c])) [0 .. ] l) [0..] ls

-- | Testing parseInput
-- >>> parseInput testInput
-- fromList [((0,0),1),((0,1),1),((0,2),2),((0,3),3),((0,4),7),((0,5),1),((0,6),1),((0,7),3),((0,8),1),((0,9),2),((1,0),1),((1,1),3),((1,2),1),((1,3),6),((1,4),4),((1,5),3),((1,6),3),((1,7),1),((1,8),2),((1,9),3),((2,0),6),((2,1),8),((2,2),3),((2,3),9),((2,4),6),((2,5),1),((2,6),5),((2,7),2),((2,8),9),((2,9),1),((3,0),3),((3,1),1),((3,2),6),((3,3),4),((3,4),3),((3,5),9),((3,6),9),((3,7),5),((3,8),3),((3,9),1),((4,0),7),((4,1),3),((4,2),5),((4,3),9),((4,4),4),((4,5),1),((4,6),9),((4,7),4),((4,8),1),((4,9),9),((5,0),5),((5,1),7),((5,2),1),((5,3),3),((5,4),1),((5,5),2),((5,6),1),((5,7),2),((5,8),3),((5,9),4),((6,0),1),((6,1),3),((6,2),1),((6,3),1),((6,4),7),((6,5),8),((6,6),2),((6,7),1),((6,8),8),((6,9),4),((7,0),7),((7,1),6),((7,2),3),((7,3),5),((7,4),1),((7,5),1),((7,6),4),((7,7),6),((7,8),5),((7,9),5),((8,0),4),((8,1),7),((8,2),2),((8,3),6),((8,4),1),((8,5),3),((8,6),2),((8,7),3),((8,8),2),((8,9),8),((9,0),2),((9,1),2),((9,2),8),((9,3),9),((9,4),1),((9,5),7),((9,6),1),((9,7),9),((9,8),1),((9,9),1)]

testInput :: String
testInput = unlines $ drop 1 $ lines [r|
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
|]


testInputBT :: String
testInputBT = unlines $ drop 1 $ lines [r|
19999
19111
11191
|]

-- >>> solve' $ parseInput testInputBT
-- (15,[(4,3),(3,3),(2,3),(1,3),(0,3),(0,2),(0,1),(0,0)])


testInput5 :: String
testInput5 = unlines $ drop 1 $ lines [r|
11637517422274862853338597396444961841755517295286
13813736722492484783351359589446246169155735727126
21365113283247622439435873354154698446526571955763
36949315694715142671582625378269373648937148475914
74634171118574528222968563933317967414442817852555
13191281372421239248353234135946434524615754563572
13599124212461123532357223464346833457545794456865
31254216394236532741534764385264587549637569865174
12931385212314249632342535174345364628545647573965
23119445813422155692453326671356443778246755488935
22748628533385973964449618417555172952866628316397
24924847833513595894462461691557357271266846838237
32476224394358733541546984465265719557637682166874
47151426715826253782693736489371484759148259586125
85745282229685639333179674144428178525553928963666
24212392483532341359464345246157545635726865674683
24611235323572234643468334575457944568656815567976
42365327415347643852645875496375698651748671976285
23142496323425351743453646285456475739656758684176
34221556924533266713564437782467554889357866599146
33859739644496184175551729528666283163977739427418
35135958944624616915573572712668468382377957949348
43587335415469844652657195576376821668748793277985
58262537826937364893714847591482595861259361697236
96856393331796741444281785255539289636664139174777
35323413594643452461575456357268656746837976785794
35722346434683345754579445686568155679767926678187
53476438526458754963756986517486719762859782187396
34253517434536462854564757396567586841767869795287
45332667135644377824675548893578665991468977611257
44961841755517295286662831639777394274188841538529
46246169155735727126684683823779579493488168151459
54698446526571955763768216687487932779859814388196
69373648937148475914825958612593616972361472718347
17967414442817852555392896366641391747775241285888
46434524615754563572686567468379767857948187896815
46833457545794456865681556797679266781878137789298
64587549637569865174867197628597821873961893298417
45364628545647573965675868417678697952878971816398
56443778246755488935786659914689776112579188722368
55172952866628316397773942741888415385299952649631
57357271266846838237795794934881681514599279262561
65719557637682166874879327798598143881961925499217
71484759148259586125936169723614727183472583829458
28178525553928963666413917477752412858886352396999
57545635726865674683797678579481878968159298917926
57944568656815567976792667818781377892989248891319
75698651748671976285978218739618932984172914319528
56475739656758684176786979528789718163989182927419
67554889357866599146897761125791887223681299833479
|]
