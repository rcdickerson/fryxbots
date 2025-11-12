module RandController
  ( RandController(..)
  , mkRandController
  ) where

import FryxbotSimulator
import System.Random

data RandController = RandController { botId :: Int
                                     , rndList :: [Int] }

mkRandController :: RandController
mkRandController = RandController { botId = 0, rndList = [] }

instance BotController RandController where

  initialize rc botId _ = rc { botId = botId
                             , rndList = randomRs (1,6) (mkStdGen botId) }

  stepBot rc st =
    let (randInt:rs) = (rndList rc)
        rc' = rc { rndList = rs }
        command = if randInt == 1 then RotateLeft
             else if randInt == 2 then RotateRight
             else if randInt == 3 then MoveForward
             else if randInt == 4 then DropBeacon (head rs)
             else if randInt == 5 then DestroyBeacon
             else Idle
    in (rc', setCommand command st)
