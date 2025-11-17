module RandController
  ( RandController(..)
  , mkRandController
  ) where

import Fryxbots.Beacon (BeaconKind(..))
import Fryxbots.Bot.Command
import Fryxbots.Bot.Controller
import Fryxbots.Bot.Facing
import Fryxbots.Bot.State
import System.Random

data RandController = RandController { botId :: Int
                                     , randGen :: StdGen }

mkRandController :: RandController
mkRandController = RandController { botId = 0, randGen = mkStdGen 0 }

instance Controller RandController where

  initialize rc botId _ = rc { botId = botId
                             , randGen = mkStdGen botId }

  stepBot rc facing sensing hasFossil st =
    let (randInt, randGen') = randomR (0, 5) (randGen rc) :: (Int, StdGen)
        rc' = rc { randGen = randGen' }
        command = if randInt == 0 then RotateLeft
             else if randInt == 1 then RotateRight
             else if randInt == 2 then MoveForward
             else if randInt == 3 then PickUpFossil
             else if randInt == 4 then DropFossil
             else Idle
    in (rc', setCommand command st)
