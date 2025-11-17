{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Fryxbots.Game
  ( Game(..)
  , executeRound
  , mkGame
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Fryxbots.Beacon (mkBeacon, BeaconKind)
import           Fryxbots.Bot (Bot)
import qualified Fryxbots.Bot as Bot
import           Fryxbots.Bot.Command
import           Fryxbots.Bot.Controller (Controller(..))
import           Fryxbots.Bot.Facing
import           Fryxbots.Field (Field)
import qualified Fryxbots.Field as Field
import           Fryxbots.Bot.Sensing (Sensing)
import qualified Fryxbots.Bot.Sensing as Sense
import           Fryxbots.Bot.State
import           Fryxbots.Pos
import           Fryxbots.Team

data Game b g where
    Game :: forall b g. (Controller b, Controller g) =>
            { field :: Field b g
            } -> Game b g

mkGame :: (Controller b, Controller g) => b -> g -> Field b g -> Game b g
mkGame blueCont goldCont field = Game {
  field = populateBots blueCont goldCont field
}

botPlacements :: (Controller b, Controller g) => Field b g -> ([Pos], [Pos])
botPlacements field = foldl addPos ([], []) allPos
  where
    allPos = [mkPos x y | x <- [0..Field.width field], y <- [0..Field.height field]]
    addPos (bs, gs) pos =
      if Field.isBlueBase field pos then (pos:bs, gs)
      else if Field.isGoldBase field pos then (bs, pos:gs)
      else (bs, gs)

populateBots :: (Controller b, Controller g) => b -> g -> Field b g -> Field b g
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

executeRound :: (Controller b, Controller g) => Game b g -> Game b g
executeRound game =
  let updateField = updateSensing . removeDeadBots . stepGoldBots . stepBlueBots
  in game { field = updateField $ field game }

stepBlueBots :: (Controller b, Controller g) => Field b g -> Field b g
stepBlueBots field = foldl step field $ Field.getBlueBots field
  where
    step :: (Controller b, Controller g) => Field b g -> Bot b -> Field b g
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
             PickUpFossil -> pickUpFossil field' bot' Field.updateBlueBot
             DropFossil -> dropFossil field' bot' Field.updateBlueBot

stepGoldBots :: (Controller b, Controller g) => Field b g -> Field b g
stepGoldBots field = foldl step field $ Field.getGoldBots field
  where
    step :: (Controller b, Controller g) => Field b g -> Bot g -> Field b g
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
             PickUpFossil -> pickUpFossil field' bot' Field.updateGoldBot
             DropFossil -> dropFossil field' bot' Field.updateGoldBot


moveBotForward :: (Controller b, Controller g) => Field b g -> Bot x -> Field b g
moveBotForward field bot =
  let botId  = Bot.id bot
      botPos = Field.lookupBotPos field botId
      newPos = adjacent (Bot.facing bot) botPos
  in if Field.isBlocked field newPos
       then field
       else Field.setBotPos field botId newPos

dropBeacon :: (Controller b, Controller g) =>
               Field b g -> Bot x -> BeaconKind -> Field b g
dropBeacon field bot kind =
  let botPos   = Field.lookupBotPos field (Bot.id bot)
      team     = Bot.team bot
      beacons  = if team == Blue then Field.blueBeacons else Field.goldBeacons
      beacons' = Map.insert botPos (mkBeacon team kind) (beacons field)
  in case team of
       Blue -> field { Field.blueBeacons = beacons' }
       Gold -> field { Field.goldBeacons = beacons' }

destroyBeacon :: (Controller b, Controller g) =>
               Field b g -> Bot x -> Field b g
destroyBeacon field bot =
  let team     = Bot.team bot
      beacons  = if team == Blue then Field.blueBeacons else Field.goldBeacons
      botPos   = Field.lookupBotPos field (Bot.id bot)
      beacons' = Map.delete botPos (beacons field)
  in case team of
       Blue -> field { Field.blueBeacons = beacons' }
       Gold -> field { Field.goldBeacons = beacons' }

removeDeadBots :: (Controller b, Controller g) => Field b g -> Field b g
removeDeadBots field =
  let deadBlues = filter (isDead field) (Field.getBlueBots field)
      deadGolds = filter (isDead field) (Field.getGoldBots field)
      removedBlues = foldl removeBot field deadBlues
  in foldl removeBot removedBlues deadGolds

removeBot :: (Controller b, Controller g) => Field b g -> Bot x -> Field b g
removeBot field bot =
  let botId  = Bot.id bot
      botPos = Field.lookupBotPos field botId
      fossilCount = Field.getFossilCount field botPos
      field' = Field.deleteBotById field botId
  in if Bot.hasFossil bot
    then Field.setFossils field' botPos $ fossilCount + 1
    else field'

isDead :: (Controller b, Controller g) => Field b g -> Bot x -> Bool
isDead field bot =
  let botId       = Bot.id bot
      botPos      = Field.lookupBotPos field botId
      botTeam     = Field.lookupBotTeam field botId
      inEnemyBase = Field.isBlueBase field botPos && botTeam == Gold
                 || Field.isGoldBase field botPos && botTeam == Blue
  in inEnemyBase

updateSensing :: (Controller b, Controller g) => Field b g -> Field b g
updateSensing field =
  let updateBlueBot bot = bot { Bot.sensing = sensingForBotId field (Bot.id bot) }
      updateGoldBot bot = bot { Bot.sensing = sensingForBotId field (Bot.id bot) }
      blueBots' = map updateBlueBot $ Field.getBlueBots field
      goldBots' = map updateGoldBot $ Field.getGoldBots field
      updatedBlues = foldl Field.updateBlueBot field blueBots'
      updatedAll = foldl Field.updateGoldBot updatedBlues goldBots'
  in updatedAll

sensingForBotId :: (Controller b, Controller g) =>
                     Field b g -> Int -> Sensing
sensingForBotId field botId =
  let botPos = Field.lookupBotPos field botId
  in Sense.Sensing
         { Sense.current = Field.scanHex field botPos
         , Sense.adjacent = \dir -> Field.scanHex field $ adjacent dir botPos
         }

pickUpFossil :: (Controller b, Controller g) =>
                     Field b g ->
                     Bot x ->
                     (Field b g -> Bot x -> Field b g) ->
                     Field b g
pickUpFossil field bot botUpdater =
  let botId = Bot.id bot
      botPos = Field.lookupBotPos field botId
      fossilCount = Field.getFossilCount field botPos
      field' = Field.setFossils field botPos (fossilCount - 1)
      bot' = bot { Bot.hasFossil = True }
  in if fossilCount > 0 && not (Bot.hasFossil bot)
       then botUpdater field' bot'
       else field

dropFossil :: (Controller b, Controller g) =>
                     Field b g ->
                     Bot x ->
                     (Field b g -> Bot x -> Field b g) ->
                     Field b g
dropFossil field bot botUpdater =
  let botId = Bot.id bot
      botPos = Field.lookupBotPos field botId
      fossilCount = Field.getFossilCount field botPos
      field' = Field.setFossils field botPos (fossilCount + 1)
      bot' = bot { Bot.hasFossil = False }
  in if Bot.hasFossil bot
       then botUpdater field' bot'
       else field
