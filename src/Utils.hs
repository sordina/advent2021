{-# LANGUAGE TypeApplications #-}

module Utils where

fixEq :: Eq t => (t -> t) -> t -> t
fixEq f x
  | x == y = x
  | otherwise = fixEq f y
  where
  y = f x