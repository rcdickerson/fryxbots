module BotCommand
  ( BotCommand(..)
  ) where

data BotCommand = Idle
                | RotateLeft
                | RotateRight
                | MoveForward
                | DropBeacon Int
                | DestroyBeacon
                | PickUpArtifact
                | DropArtifact
