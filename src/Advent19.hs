module Advent19 where

day19 = undefined
day19b = undefined

-- hoe2 -m 'Text.Parsec Debug.Trace' '
-- let
-- foo :: [(String,[[String]])] -> Parsec String () ()
-- foo r =
-- 	let
-- 	a = M.fromList (map g r)
-- 	f n@(x:xs)
-- 		| isDigit x = a ! n
-- 		| otherwise = () <$ string (init xs)
-- 	g = second (choice . map (try . mapM_ f))
-- 	m ! k = fromJust $ M.lookup k m
-- 	in a ! "0" *> eof
-- in
-- length .  rights
-- . (\(s,p) -> map (parse p "rules") s)
-- . (last &&& foo . map ((init . head &&& splitOn ["|"] . tail) . words) .  head)
-- . splitOn [""]
-- '

-- #!/bin/sh


-- hoe2 -m 'Control.Monad.Trans.Free Control.Monad.State Data.Tree' '
-- let

-- token            = FreeT . pure . Free $ FreeT . pure . Pure
-- parseStream next = runStateT . iterTM (StateT next >>=)
-- parseString      = parseStream (maybe empty pure . Data.List.uncons)
-- char c           = mfilter (==c) token
-- string           = mapM char
-- choice           = foldl (<|>) empty

-- sub t@("8", _) = t & _2 .~ [["42"],["42","8"]]
-- sub t@("11",_) = t & _2 .~ [["42","31"],["42","11","31"]]
-- sub t          = t

-- m!k = fromJust $ M.lookup k m

-- n :: [a] -> Bool
-- n = null

-- build :: [(String,[[String]])] -> FreeT ((->) Char) [] (Tree String)
-- build r = fmap (Node "0") $ a ! "0" <* string "EOF"
-- 	where
-- 	a = M.fromList (map (second g) r)
-- 	g = choice . map (traverse f)
-- 	f n@(x:xs)
-- 		| isDigit x  = Node n <$> a ! n
-- 		| otherwise  = Node n [] <$ string (init xs)
-- in
-- (\x -> map drawTree x ++ [show $ length x])
-- . map (fst . head)
-- . filter (not . null)
-- . (\(s,p) -> map (parseString p) s)
-- . (map (++"EOF") . last &&& build . map (sub . (init . head &&& splitOn ["|"] . tail) . words) .  head)
-- . splitOn [""]
-- '
