{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Fryxbot
  ( Fryxbot(..)
  , invokeController
  , mkBot
  ) where

import           BotController
import           BotFacing
import           BotSensing
import           BotState (BotState)
import qualified BotState as St
import           Prelude hiding (id)
import           Team

data Fryxbot c where
  Fryxbot :: forall c. BotController c =>
             { id :: Int
             , facing :: BotFacing
             , sensing :: BotSensing
             , state :: BotState
             , team :: Team
             , controller :: c
             } -> Fryxbot c

mkBot :: BotController c => Int -> Team -> c -> Fryxbot c
mkBot id team controller = Fryxbot
  { id = id
  , facing = NorthEast
  , sensing = emptySensing
  , state = St.empty
  , team = team
  , controller = controller
  }

invokeController :: BotController c => Fryxbot c -> Fryxbot c
invokeController bot =
  let (cont', state') = stepBot (controller bot) (state bot)
  in bot { state = state', controller = cont' }
