module BotState
  ( BotState(..)
  , empty
  , memStore
  , memRead
  , setCommand
  ) where

import           BotCommand
import           BotFacing
import           Data.Map (Map)
import qualified Data.Map as Map

data BotState = BotState
  { command :: BotCommand
  , memory  :: Map String Int
  }

empty :: BotState
empty = BotState
  { command = Idle
  , memory = Map.empty
  }

setCommand :: BotCommand -> BotState -> BotState
setCommand cmd st = st { command = cmd }

memStore :: String -> Int -> BotState -> BotState
memStore varName val st =
  st { memory = Map.insert varName val $ memory st }

memRead :: String -> BotState -> Maybe Int
memRead varName = Map.lookup varName . memory
