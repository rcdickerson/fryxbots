module Team (
  Team(..)
  ) where

data Team = Blue | Gold
   deriving(Ord, Eq, Show)
