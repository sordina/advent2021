module Advent25 where

day25 = undefined

-- hoe2 "
-- let
-- loop s n = mod (n*s) 20201227
-- in
-- (\v@((l1,k1),(l2,k2)) -> iterate (loop k1) 1 !! l2)
-- .
-- (head &&& head . tail)
-- .
-- map (\x -> head $ filter ((==x) . snd) $ zip [0..] $ iterate (loop 7) 1)
-- .
-- map (read :: String -> Int) .  words
-- "
