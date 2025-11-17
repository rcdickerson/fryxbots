{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Fryxbots.Bot
  ( Bot(..)
  , invokeController
  , mkBot
  ) where

import           Fryxbots.Bot.Controller
import           Fryxbots.Bot.Facing
import           Fryxbots.Bot.Sensing
import           Fryxbots.Bot.State (State)
import qualified Fryxbots.Bot.State as St
import           Fryxbots.Team
import           Prelude hiding (id)

data Bot c where
  Bot :: forall c. Controller c =>
          { id :: Int
          , facing :: Facing
          , sensing :: Sensing
          , state :: State
          , team :: Team
          , hasFossil :: False
          , controller :: c
          } -> Bot c

mkBot :: Controller c => Int -> Team -> c -> Bot c
mkBot id team controller = Bot
  { id = id
  , facing = NorthEast
  , sensing = emptySensing
  , state = St.empty
  , team = team
  , hasFossil = True
  , controller = controller
  }

invokeController :: Controller c => Bot c -> Bot c
invokeController bot =
  let (cont', state') = stepBot (controller bot) (facing bot) (sensing bot) (hasFossil bot) (state bot)
  in bot { state = state', controller = cont' }
