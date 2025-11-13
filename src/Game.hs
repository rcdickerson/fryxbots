{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Game
  ( Game(..)
  , executeRound
  , mkGame
  ) where

import           Beacon (mkBeacon, BeaconKind)
import           BotCommand
import           BotController (BotController(..))
import           BotFacing
import           BotState
import           Data.Map (Map)
import qualified Data.Map as Map
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
      afterBlueMoves = foldl stepBlueBot (field game) blueBots
      afterGoldMoves = foldl stepGoldBot afterBlueMoves goldBots
      cleanedField = removeDeadBots afterGoldMoves
  in game { field = cleanedField }

stepBlueBot :: (BotController b, BotController g) => Field b g -> Fryxbot b -> Field b g
stepBlueBot field bot =
  let botId  = Bot.id bot
      bot'   = Bot.invokeController bot
      field' = Field.updateBlueBot field bot'
  in case (command . Bot.state) bot' of
    Idle        -> field'
    RotateLeft  -> Field.updateBlueBot field' $
                     bot' { Bot.facing = rotateLeft $ Bot.facing bot' }
    RotateRight -> Field.updateBlueBot field' $
                     bot' { Bot.facing = rotateRight $ Bot.facing bot' }
    MoveForward -> moveBotForward field' bot'
    DropBeacon kind -> dropBeacon field' bot' kind
    DestroyBeacon -> destroyBeacon field' bot'
    PickUpFossil -> field'
    DropFossil -> field'

stepGoldBot :: (BotController b, BotController g) => Field b g -> Fryxbot g -> Field b g
stepGoldBot field bot =
  let botId  = Bot.id bot
      bot'   = Bot.invokeController bot
      field' = Field.updateGoldBot field bot'
  in case (command . Bot.state) bot' of
    Idle        -> field'
    RotateLeft  -> Field.updateGoldBot field' $
                     bot' { Bot.facing = rotateLeft $ Bot.facing bot' }
    RotateRight -> Field.updateGoldBot field' $
                     bot' { Bot.facing = rotateRight $ Bot.facing bot' }
    MoveForward -> moveBotForward field' bot'
    DropBeacon kind -> dropBeacon field' bot' kind
    DestroyBeacon -> destroyBeacon field' bot'
    PickUpFossil -> field'
    DropFossil -> field'


moveBotForward :: (BotController b, BotController g) => Field b g -> Fryxbot x -> Field b g
moveBotForward field bot =
  let botId  = Bot.id bot
      botPos = Field.lookupBotPos field botId
      newPos = adjacent (Bot.facing bot) botPos
  in if Field.isBlocked field newPos
       then field
       else Field.setBotPos field botId newPos

dropBeacon :: (BotController b, BotController g) =>
               Field b g -> Fryxbot x -> BeaconKind -> Field b g
dropBeacon field bot kind =
  let botPos   = Field.lookupBotPos field (Bot.id bot)
      team     = Bot.team bot
      beacons' = Map.insert botPos (mkBeacon team kind) (Field.beacons field)
  in field { Field.beacons = beacons' }

destroyBeacon :: (BotController b, BotController g) =>
               Field b g -> Fryxbot x -> Field b g
destroyBeacon field bot =
  let botPos   = Field.lookupBotPos field (Bot.id bot)
      beacons' = Map.delete botPos (Field.beacons field)
   in field { Field.beacons = beacons' }

removeDeadBots :: (BotController b, BotController g) => Field b g -> Field b g
removeDeadBots field =
  let deadBlueIds = map (Bot.id) $ filter (isDead field) (Field.getBlueBots field)
      deadGoldIds = map (Bot.id) $ filter (isDead field) (Field.getGoldBots field)
  in foldl Field.deleteBotById field $ deadBlueIds ++ deadGoldIds

isDead :: (BotController b, BotController g) => Field b g -> Fryxbot x -> Bool
isDead field bot =
  let botId       = Bot.id bot
      botPos      = Field.lookupBotPos field botId
      botTeam     = Field.lookupBotTeam field botId
      inEnemyBase = Field.isBlueBase field botPos && botTeam == Gold
                 || Field.isGoldBase field botPos && botTeam == Blue
  in inEnemyBase
