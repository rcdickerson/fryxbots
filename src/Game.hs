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
import           BotSensing (BotSensing)
import qualified BotSensing as Sense
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
      addBlueBot field' botId pos =
          Field.addBlueBot field' (Bot.mkBot botId Blue $ initialize blueCont botId Blue) pos
      addGoldBot field' botId pos =
          Field.addGoldBot field' (Bot.mkBot botId Gold $ initialize goldCont botId Gold) pos
      withBlues = foldl (\fld (botId, pos) -> addBlueBot fld botId pos) field idedBlues
      withBoth = foldl (\fld (botId, pos) -> addGoldBot fld botId pos) withBlues idedGolds
  in withBoth

executeRound :: (BotController b, BotController g) => Game b g -> Game b g
executeRound game =
  let updateField = updateSensing . removeDeadBots . stepGoldBots . stepBlueBots
  in game { field = updateField $ field game }

stepBlueBots :: (BotController b, BotController g) => Field b g -> Field b g
stepBlueBots field = foldl step field $ Field.getBlueBots field
  where
    step :: (BotController b, BotController g) => Field b g -> Fryxbot b -> Field b g
    step field bot =
        let bot'   = Bot.invokeController bot
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

stepGoldBots :: (BotController b, BotController g) => Field b g -> Field b g
stepGoldBots field = foldl step field $ Field.getGoldBots field
  where
    step :: (BotController b, BotController g) => Field b g -> Fryxbot g -> Field b g
    step field bot =
        let bot'   = Bot.invokeController bot
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
      beacons  = if team == Blue then Field.blueBeacons else Field.goldBeacons
      beacons' = Map.insert botPos (mkBeacon team kind) (beacons field)
  in case team of
       Blue -> field { Field.blueBeacons = beacons' }
       Gold -> field { Field.goldBeacons = beacons' }

destroyBeacon :: (BotController b, BotController g) =>
               Field b g -> Fryxbot x -> Field b g
destroyBeacon field bot =
  let team     = Bot.team bot
      beacons  = if team == Blue then Field.blueBeacons else Field.goldBeacons
      botPos   = Field.lookupBotPos field (Bot.id bot)
      beacons' = Map.delete botPos (beacons field)
  in case team of
       Blue -> field { Field.blueBeacons = beacons' }
       Gold -> field { Field.goldBeacons = beacons' }

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

updateSensing :: (BotController b, BotController g) => Field b g -> Field b g
updateSensing field =
  let updateBlueBot bot = bot { Bot.sensing = sensingForBotId field (Bot.id bot) }
      updateGoldBot bot = bot { Bot.sensing = sensingForBotId field (Bot.id bot) }
      blueBots' = map updateBlueBot $ Field.getBlueBots field
      goldBots' = map updateGoldBot $ Field.getGoldBots field
      updatedBlues = foldl Field.updateBlueBot field blueBots'
      updatedAll = foldl Field.updateGoldBot updatedBlues goldBots'
  in updatedAll

sensingForBotId :: (BotController b, BotController g) =>
                     Field b g -> Int -> BotSensing
sensingForBotId field botId =
  let botPos = Field.lookupBotPos field botId
  in Sense.BotSensing
         { Sense.current = Field.scanHex field botPos
         , Sense.adjacent = \dir -> Field.scanHex field $ adjacent dir botPos
         }
