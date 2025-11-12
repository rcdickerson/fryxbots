module BotController
( BotController(..)
) where

import BotState
import Team

class BotController a where
  initialize :: a -> Int -> Team -> a
  stepBot :: a -> BotState -> (a, BotState)
