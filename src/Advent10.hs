module Advent10 where

day10 = undefined
day10b = undefined


-- hoe 'product.map length.group.filter (flip elem [1,3]).sort.map (uncurry (-)).uncurry zip.(tail &&& id).sort.(\x -> [0] ++ x ++ [maximum x + 3]).map read'

-- #!/bin/sh

-- # see https://crypto.stanford.edu/pbc/notes/zdd/zdd.html

-- hoe2 -m Data.Array '
-- let
-- 	edges p l = q =<< tails l
-- 			where
-- 			q (x:xs) = map (x,) $ takeWhile (p x) xs
-- 			q _ = []
-- 	paths f t l =
-- 			let
-- 					a = array (f,t) ((t,1) : [(x, sum $ map (a!) (map snd $ filter ((==x) . fst) l)) | x <- [f.. pred t]])
-- 			in a ! f
-- in
-- 	(\x -> let m = maximum x + 3 in paths 0 m $ edges (\a b -> b <= a + 3) (0 : x ++ [m]))
-- 	.  sort .  map read
-- '

