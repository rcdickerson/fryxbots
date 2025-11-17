module Fryxbots.Bot.Controller
( Controller(..)
) where

import Fryxbots.Bot.Facing
import Fryxbots.Bot.Sensing
import Fryxbots.Bot.State
import Fryxbots.Team

class Controller a where
  initialize :: a -> Int -> Team -> a
  stepBot :: a -> Facing -> Sensing -> Bool -> State -> (a, State)
