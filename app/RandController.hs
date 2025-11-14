module RandController
  ( RandController(..)
  , mkRandController
  ) where

import FryxbotWars
import System.Random

data RandController = RandController { botId :: Int
                                     , rndList :: [Int] }

mkRandController :: RandController
mkRandController = RandController { botId = 0, rndList = [] }

instance BotController RandController where

  initialize rc botId _ = rc { botId = botId
                             , rndList = randomRs (0,5) (mkStdGen botId) }

  stepBot rc facing sensing st =
    let (randInt:rs) = (rndList rc)
        rc' = rc { rndList = rs }
        command = if randInt == 0 then RotateLeft
             else if randInt == 1 then RotateRight
             else if randInt == 2 then MoveForward
             else if randInt == 3 then DropBeacon $
                  [Kind1, Kind2, Kind3, Kind4, Kind5, Kind6] !! (head rs)
             else if randInt == 4 then DestroyBeacon
             else Idle
    in (rc', setCommand command st)
