module Fryxbots.Beacon
  ( Beacon(..)
  , BeaconKind(..)
  , mkBeacon
  ) where

import Fryxbots.Team

data Beacon = Beacon
  { team :: Team
  , kind :: BeaconKind
  }

data BeaconKind = Kind1
                | Kind2
                | Kind3
                | Kind4
                | Kind5
                | Kind6
                deriving (Eq, Ord, Show)

mkBeacon :: Team -> BeaconKind -> Beacon
mkBeacon team kind = Beacon { team = team, kind = kind }
