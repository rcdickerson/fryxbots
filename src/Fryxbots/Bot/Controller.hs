module Fryxbots.Bot.Controller
( Controller(..)
) where

import Fryxbots.Bot.Facing
import Fryxbots.Bot.Sensing
import Fryxbots.Bot.State
import Fryxbots.Team

class Controller a where

  -- Initialize the controller. Takes the controller,
  -- the integer ID of the bot the controller is being
  -- initialized for, and the team the bot is on.
  initialize :: a -> Int -> Team -> a

  -- Step to a new state. Arguments give:
  --
  --  + The controller
  --  + The direction the bot is facing
  --  + The hex scans for the current and adjacent positions
  --  + Whether or not the bot is carrying a fossile
  --  + The current state
  --
  stepBot :: a -> Facing -> Sensing -> Bool -> State -> (a, State)
