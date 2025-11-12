{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Game
  ( Game(..)
  , executeRound
  , mkGame
  ) where

import           BotCommand
import           BotController (BotController(..))
import           BotFacing
import           BotState
import           Field (Field)
import qualified Field as Field
import           Fryxbot (Fryxbot)
import qualified Fryxbot as Bot
import           Pos
import           Team

data Game b g where
    Game :: forall b g. (BotController b, BotController g) =>
            { field :: Field b g
            } -> Game b g

mkGame :: (BotController b, BotController g) => b -> g -> Field b g -> Game b g
mkGame blueCont goldCont field = Game {
  field = populateBots blueCont goldCont field
}

botPlacements :: (BotController b, BotController g) => Field b g -> ([Pos], [Pos])
botPlacements field = foldl addPos ([], []) allPos
  where
    allPos = [mkPos x y | x <- [0..Field.width field], y <- [0..Field.height field]]
    addPos (bs, gs) pos =
      if Field.isBlueBase field pos then (pos:bs, gs)
      else if Field.isGoldBase field pos then (bs, pos:gs)
      else (bs, gs)

populateBots :: (BotController b, BotController g) => b -> g -> Field b g -> Field b g
populateBots blueCont goldCont field =
  let (blues, golds) = botPlacements field
      idedBlues = zip [0..] blues
      idedGolds = zip [length blues..] golds
      addBlueBot field botId pos =
          Field.addBlueBot field (Bot.mkBot botId Blue $ initialize blueCont botId Blue) pos
      addGoldBot field botId pos =
          Field.addGoldBot field (Bot.mkBot botId Gold $ initialize goldCont botId Gold) pos
      withBlues = foldl (\fld (botId, pos) -> addBlueBot fld botId pos) field idedBlues
      withBoth = foldl (\fld (botId, pos) -> addGoldBot fld botId pos) withBlues idedGolds
  in withBoth

executeRound :: (BotController b, BotController g) => Game b g -> Game b g
executeRound game =
  let blueBots = (Field.getBlueBots . field) game
      goldBots = (Field.getGoldBots . field) game
      game' = game { field = foldl Field.stepBlueBot (field game) blueBots }
  in game' { field = foldl Field.stepGoldBot (field game') goldBots }
