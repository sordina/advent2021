module Advent16 where

day16 = undefined
day16b = undefined

-- hoe2 '
-- let
-- mkRule r n = any (\(a,b) -> n >= a && n <= b) r
-- rules r x = none ($ x) (map mkRule r)
-- in
-- sum
-- .
-- (\(r,l) -> filter (rules r) l)
-- .
-- (
-- map (map ((head &&& last) . map (read @Int)) . filter ((==2) . length) . map (splitOn "-") . words) . head
-- &&&
-- concatMap (read @[Int] . (\x -> "["++x++"]")) . tail . last
-- )
-- .
-- splitOn [""]
-- '

-- #!/bin/sh

-- hoe2 '
-- let

-- reduce l    = map (\m -> filter (\n-> sing m || (not (elem n (concat $ filter sing l)))) m) l
-- sing        = (==1) . length
-- fixf f      = fst . head . dropWhile (uncurry (/=)) . uncurry zip . (id &&& tail) . iterate f
-- r2 l        = let (a,b) = unzip l in zip a $ reduce b
-- mkRule r n  = any (\(a,b) -> n >= a && n <= b) r
-- rules r x   = none ($ x) (map mkRule r)
-- f a l (n,r) = (n, filter (\i -> all (mkRule r) (map (!!i) l)) a)
-- q           = map (read @[Int] . (\x -> "["++x++"]")) . tail

-- in
-- product
-- .
-- (\(m,l) -> map (head m!!) (concatMap snd l))
-- .
-- (id *** filter (isPrefixOf "departure " . fst) . fixf r2)
-- .
-- (\(m, r,l) -> (m, map (f [0..pred (length r)] l) r))
-- .
-- (\(r,(l,m)) -> (m, r, filter (none (rules (map snd r))) l))
-- .
-- (
-- 	map
-- 		(
-- 			takeWhile (not . flip elem ":")
-- 			&&&
-- 			map ((head &&& last) . map (read @Int))
-- 			. filter ((==2) . length) . map (splitOn "-") . words
-- 		) . head
-- 	&&&
-- 	q . last &&& q . (!!1)
-- )
-- .
-- splitOn [""]
-- '
