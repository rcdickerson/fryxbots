module BotSensing
  ( BotSensing(..)
  , HexScan(..)
  , emptyScan
  , emptySensing
  ) where

import           Beacon
import           BotFacing (BotFacing)
import qualified BotFacing as Facing
import           Team

data BotSensing = BotSensing
  { current :: HexScan
  , adjacent :: BotFacing -> HexScan
  }

emptySensing :: BotSensing
emptySensing = BotSensing
  { current = emptyScan
  , adjacent = \_ -> emptyScan
  }

data HexScan = OffMap | HexScan
  { beacon :: Team -> Maybe BeaconKind
  , isBuilding :: Bool
  , isBase :: Team -> Bool
  , numFossils :: Int
  }

emptyScan :: HexScan
emptyScan = HexScan
  { beacon = \_ -> Nothing
  , isBuilding = False
  , isBase = \_ -> False
  , numFossils = 0
  }
