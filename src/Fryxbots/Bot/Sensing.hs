module Fryxbots.Bot.Sensing
  ( Sensing(..)
  , HexScan(..)
  , emptyScan
  , emptySensing
  ) where

import           Fryxbots.Beacon
import           Fryxbots.Bot.Facing (Facing)
import qualified Fryxbots.Bot.Facing as Facing
import           Fryxbots.Team

data Sensing = Sensing
  { current :: HexScan
  , adjacent :: Facing -> HexScan
  }

emptySensing :: Sensing
emptySensing = Sensing
  { current = emptyScan
  , adjacent = \_ -> emptyScan
  }

data HexScan = OffMap | HexScan
  { beacon :: Team -> Maybe BeaconKind
  , isBuilding :: Bool
  , isBase :: Team -> Bool
  , hasBot :: Team -> Bool
  , numFossils :: Int
  }

emptyScan :: HexScan
emptyScan = HexScan
  { beacon = \_ -> Nothing
  , isBuilding = False
  , isBase = \_ -> False
  , hasBot = \_ -> False
  , numFossils = 0
  }
