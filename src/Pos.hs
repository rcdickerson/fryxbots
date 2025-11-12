module Pos
  ( Pos(..)
  , mkPos
  ) where

data Pos = Pos
  { posX :: Int
  , posY :: Int
  } deriving(Eq, Ord, Show)

mkPos :: Int -> Int -> Pos
mkPos x y = Pos { posX = x, posY = y }
