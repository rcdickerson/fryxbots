module BotController
( BotController(..)
) where

import BotFacing
import BotSensing
import BotState
import Team

class BotController a where
  initialize :: a -> Int -> Team -> a
  stepBot :: a -> BotFacing -> BotSensing -> BotState -> (a, BotState)
