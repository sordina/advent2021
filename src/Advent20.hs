module Advent20 where

day20 = undefined
day20b = undefined

-- hoe -m 'Data.Bool' '
-- let

-- match     x = and $ zipWith aligned x (tail x)
-- aligned a b = and $ zipWith (==) (map last a) (map head b)
-- check     x = all match $ (map (map snd)) $ x ++ transpose (map (map (second transpose)) x)
-- square    x = chunksOf (side x) x
-- side      x = floor (sqrt (fromIntegral (length x)))

-- accept m n p q y
-- 	| n == 0    = True
-- 	| m         = aligned (transpose (snd q)) (transpose (snd y))
-- 	| otherwise = aligned (snd p) (snd y)

-- permute s n p q [] = [[]]
-- permute s n p q xs =
-- 	let m = mod n s == 0
-- 	in [y : ys | x <- xs, y <- orient x, accept m n p q y, ys <- permute s (succ n) y (bool q y m) (delete x xs)]

-- res    x = product $ map fst [ head $ head x , last $ head x , head $ last x , last $ last x ]
-- orient x = flip x ++ flip (second transpose x)
-- flip   x = map (\f -> second f x) [ id , reverse , map reverse , reverse . map reverse ]

-- in
-- res
-- . head
-- . filter check
-- . map square
-- . (\x -> permute (side x) 0 undefined undefined x)
-- . map (read . filter isDigit . head &&& tail)
-- . splitOn [""]
-- '

-- #!/bin/sh

-- hoe2 -m 'Data.Bool' '
-- let

-- match     x = and $ zipWith aligned x (tail x)
-- aligned a b = and $ zipWith (==) (map last a) (map head b)
-- check     x = all match $ (map (map snd)) $ x ++ transpose (map (map (second transpose)) x)
-- square    x = chunksOf (side x) x
-- side      x = floor (sqrt (fromIntegral (length x)))

-- accept m n p q y
-- 	| n == 0    = True
-- 	| m         = aligned (transpose (snd q)) (transpose (snd y))
-- 	| otherwise = aligned (snd p) (snd y)

-- permute s n p q [] = [[]]
-- permute s n p q xs =
-- 	let m = mod n s == 0
-- 	in [y : ys | x <- xs, y <- orient x, accept m n p q y, ys <- permute s (succ n) y (bool q y m) (delete x xs)]

-- res    x = product $ map fst [ head $ head x , last $ head x , head $ last x , last $ last x ]
-- orient x = flop x ++ flop (second reverse x)
-- flop   x = map (flip second x) [ id , rleft, rright, reverse . map reverse ]
-- rleft    = reverse . transpose
-- rright   = map reverse . transpose
-- stitch   = concatMap $ foldr1 (zipWith (++)) . map (init . tail . map (init . tail) . snd)
-- monster  = [ "                  # " , "#    ##    ##    ###" , " #  #  #  #  #  #   " ]

-- replaceMonster s = foldr (zipWith2dx replace) s (matches2d monster s)
-- 	where
-- 	replace a b
-- 		| elem a "#" = head "O"
-- 		| otherwise  = b

-- matches2d p s = [o | y <- [0.. hs-hp], x <- [0.. ws-wp], o <- [offset2d (x,y) p], and $ map and $ zipWith2d f o s]
-- 	where
-- 	hs = length s
-- 	hp = length p
-- 	ws = length (head s)
-- 	wp = length (head p)
-- 	f a b | elem a "#" = elem b "#"
-- 	      | otherwise  = True

-- zipWith2d  f   = zipWith (zipWith f) -- faster version for matching
-- zipWith2dx f a = zipWith2d f (map (++ spc) a ++ repeat spc)

-- spc = cycle " "

-- offset2d (x,y) = (replicate y "" ++) . map (take x spc ++)

-- in

-- minimum
-- . map (sum . map (length . filter (flip elem "#")) . replaceMonster . snd)
-- . orient
-- . (undefined,)
-- . stitch
-- . head
-- . filter check
-- . map square
-- . (\x -> permute (side x) 0 undefined undefined x)
-- . map (head &&& tail)
-- . splitOn [""]
-- '