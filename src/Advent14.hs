module Advent14 where

day14 = undefined
day14b = undefined

-- hoe2 -m 'Control.Lens.Operators Control.Monad.State' "
-- let
-- 	f :: String -> State ([Integer -> Integer], M.Map Integer Integer) ()
-- 	f l@('m':'a':_) = _1 .= (reverse $ c $ last $ words l)
-- 	f l = do
-- 			a <- _1 <%= id
-- 			_2 %= M.insert m (d (zipWith ($) a n))
-- 		where
-- 		m = r $ takeWhile isDigit $ dropWhile (not . isDigit) l
-- 		n = b $ r $ last $ words l

-- 	r = read :: String -> Integer

-- 	b 0 = repeat 0
-- 	b x = mod x 2 : b (div x 2)

-- 	d [] = 0
-- 	d (x:xs) = x + 2 * (d xs)

-- 	c = map (fromJust . flip lookup [('X',id),('1',const 1),('0',const 0)])
-- in
-- sum
-- .
-- snd
-- .
-- (([],M.empty) &~)
-- .
-- mapM_ f
-- .
-- lines
-- "
-- #!/bin/sh

-- hoe2 -m 'Control.Lens.Operators Control.Monad.State' "
-- let
-- 	f :: String -> State ([[Integer -> Integer]], M.Map Integer Integer) ()
-- 	f l@('m':'a':_) = _1 .= (map reverse $ c $ last $ words l)
-- 	f l = do
-- 			a <- _1 <%= id
-- 			mapM_ (\x -> _2 %= M.insert (d (zipWith ($) x m)) n) a
-- 		where
-- 		m = b $ r $ takeWhile isDigit $ dropWhile (not . isDigit) l
-- 		n = r $ last $ words l

-- 	r = read :: String -> Integer

-- 	b 0 = repeat 0
-- 	b x = mod x 2 : b (div x 2)

-- 	d [] = 0
-- 	d (x:xs) = x + 2 * (d xs)

-- 	c = sequence . map (fromJust . flip lookup [('X',[const 0, const 1]),('1',[const 1]),('0',[id])])
-- in
-- sum
-- .
-- snd
-- .
-- (([],M.empty) &~)
-- .
-- mapM_ f
-- .
-- lines
-- "

