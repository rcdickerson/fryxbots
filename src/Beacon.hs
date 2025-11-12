module Beacon
  ( Beacon(..)
  , mkBeacon
  ) where

import Team

data Beacon = Beacon
  { team :: Team
  , kind :: Int
  }

mkBeacon :: Team -> Int -> Beacon
mkBeacon team kind = Beacon { team = team, kind = kind }
