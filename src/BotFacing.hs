module BotFacing
  ( BotFacing(..)
  , adjacent
  , rotateLeft
  , rotateRight
  ) where

import Pos

data BotFacing = NorthEast
               | East
               | SouthEast
               | SouthWest
               | West
               | NorthWest

rotateLeft :: BotFacing -> BotFacing
rotateLeft facing = case facing of
  NorthEast -> NorthWest
  East -> NorthEast
  SouthEast -> East
  SouthWest -> SouthEast
  West -> SouthWest
  NorthWest -> West

rotateRight :: BotFacing -> BotFacing
rotateRight facing = case facing of
  NorthEast -> East
  East -> SouthEast
  SouthEast -> SouthWest
  SouthWest -> West
  West -> NorthWest
  NorthWest -> NorthEast

adjacent :: BotFacing -> Pos -> Pos
adjacent facing pos = case facing of
  NorthEast -> pos { posX = (posX pos) + 1
                   , posY = (posY pos) - 1 }
  East      -> pos { posX = (posX pos) + 1 }
  SouthEast -> pos { posX = (posX pos) + 1
                   , posY = (posY pos) + 1 }
  SouthWest -> pos { posX = (posX pos) - 1
                   , posY = (posY pos) + 1 }
  West      -> pos { posX = (posX pos) - 1 }
  NorthWest -> pos { posX = (posX pos) - 1
                   , posY = (posY pos) - 1 }
