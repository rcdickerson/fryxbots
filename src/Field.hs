{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Field
  ( BotController(..)
  , CellKind(..)
  , Field(..)
  , addBlueBot
  , addGoldBot
  , cellKind
  , getBlueBots
  , getGoldBots
  , isBlueBase
  , isGoldBase
  , lookupBotPos
  , mkField
  , setArtifacts
  , setBotPos
  , setBlueBase
  , setBuilding
  , setGoldBase
  , stepBlueBot
  , stepGoldBot
  , showField
  , updateBlueBot
  , updateGoldBot
  ) where

import           Beacon
import           BotCommand
import           BotController
import           BotFacing
import           BotState
import           Data.Char (intToDigit)
import           Data.List (intercalate, intersperse)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Fryxbot (Fryxbot)
import qualified Fryxbot as Bot
import           Pos
import           Team

data Field b g where
  Field :: forall b g. (BotController b, BotController g) =>
           { width          :: Int
           , height         :: Int
           , blueBotsById   :: Map Int (Fryxbot b)
           , goldBotsById   :: Map Int (Fryxbot g)
           , botsByPosition :: Map Pos Int
           , positionsByBot :: Map Int Pos
           , artifacts      :: Map Pos Int
           , buildings      :: Set Pos
           , blueBase       :: Set Pos
           , goldBase       :: Set Pos
           , beacons        :: Map Pos Beacon
           } -> Field b g

mkField :: (BotController b, BotController g) => Int -> Int -> Field b g
mkField width height = Field
  { width = width
  , height = height
  , blueBotsById = Map.empty
  , goldBotsById = Map.empty
  , botsByPosition = Map.empty
  , positionsByBot = Map.empty
  , artifacts = Map.empty
  , buildings = Set.empty
  , blueBase = Set.empty
  , goldBase = Set.empty
  , beacons = Map.empty
  }

data CellKind = Building
              | BlueBase
              | GoldBase
              | BlueBot BotFacing
              | GoldBot BotFacing
              | BlueBeacon Int
              | GoldBeacon Int
              | Artifacts Int
              | Open

setBuilding :: (BotController b, BotController g) => Field b g -> Pos -> Field b g
setBuilding field pos = field {
  buildings = Set.insert pos $ buildings field
}

setBlueBase :: (BotController b, BotController g) => Field b g -> Pos -> Field b g
setBlueBase field pos = field {
  blueBase = Set.insert pos $ blueBase field
}

setGoldBase :: (BotController b, BotController g) => Field b g -> Pos -> Field b g
setGoldBase field pos = field {
  goldBase = Set.insert pos $ goldBase field
}

setArtifacts :: (BotController b, BotController g) => Field b g -> Pos -> Int -> Field b g
setArtifacts field pos i = field {
  artifacts = Map.insert pos i $ artifacts field
}

isBlueBase :: (BotController b, BotController g) => Field b g -> Pos -> Bool
isBlueBase field pos = Set.member pos $ blueBase field

isGoldBase :: (BotController b, BotController g) => Field b g -> Pos -> Bool
isGoldBase field pos = Set.member pos $ goldBase field

addBlueBot :: (BotController b, BotController g) => Field b g -> Fryxbot b -> Pos -> Field b g
addBlueBot field bot pos =
  let botId = Bot.id bot
  in
    if Map.member botId (blueBotsById field) then
      error $ "Bot ID already registered: " ++ (show botId)
    else field
      { blueBotsById = Map.insert botId bot $ blueBotsById field
      , botsByPosition = Map.insert pos botId $ botsByPosition field
      , positionsByBot = Map.insert botId pos $ positionsByBot field
      }

addGoldBot :: (BotController b, BotController g) => Field b g -> Fryxbot g -> Pos -> Field b g
addGoldBot field bot pos =
  let botId = Bot.id bot
  in
    if Map.member botId (goldBotsById field) then
      error $ "Bot ID already registered: " ++ (show botId)
    else field
      { goldBotsById = Map.insert botId bot $ goldBotsById field
      , botsByPosition = Map.insert pos botId $ botsByPosition field
      , positionsByBot = Map.insert botId pos $ positionsByBot field
      }

updateBlueBot :: (BotController b, BotController g) => Field b g -> Fryxbot b -> Field b g
updateBlueBot field bot =
  let botId = Bot.id bot
      botsMap = blueBotsById field
  in if Map.member botId botsMap
     then field { blueBotsById = Map.insert botId bot botsMap }
     else error $ "No bot registered with ID: " ++ show botId

updateGoldBot :: (BotController b, BotController g) => Field b g -> Fryxbot g -> Field b g
updateGoldBot field bot =
  let botId = Bot.id bot
  in case Map.lookup botId (goldBotsById field) of
    Nothing  -> error $ "No bot registered with ID: " ++ show botId
    Just _ -> field { goldBotsById = Map.insert botId bot $ goldBotsById field }

getBlueBots :: (BotController b, BotController g) => Field b g -> [Fryxbot b]
getBlueBots field = Map.elems $ blueBotsById field

getGoldBots :: (BotController b, BotController g) => Field b g -> [Fryxbot g]
getGoldBots field = Map.elems $ goldBotsById field

lookupBotPos :: (BotController b, BotController g) => Field b g -> Int -> Pos
lookupBotPos field botId = case Map.lookup botId $ positionsByBot field of
  Nothing  -> error $ "No bot registered with ID: " ++ show botId
  Just pos -> pos

isBlueBotAt :: (BotController b, BotController g) => Field b g -> Pos -> Bool
isBlueBotAt field pos = case Map.lookup pos (botsByPosition field) of
  Nothing -> False
  Just botId -> Map.member botId (blueBotsById field)

isGoldBotAt :: (BotController b, BotController g) => Field b g -> Pos -> Bool
isGoldBotAt field pos = case Map.lookup pos (botsByPosition field) of
  Nothing -> False
  Just botId -> Map.member botId (goldBotsById field)

isBuildingAt :: (BotController b, BotController g) => Field b g -> Pos -> Bool
isBuildingAt field pos = Set.member pos (buildings field)

isBlocked :: (BotController b, BotController g) => Field b g -> Pos -> Bool
isBlocked field pos = or $ map ($ pos) [isBuildingAt field, isBlueBotAt field, isGoldBotAt field]

setBotPos :: (BotController b, BotController g) => Field b g -> Int -> Pos -> Field b g
setBotPos field botId pos =
  case Map.lookup botId (positionsByBot field) of
    Nothing -> error $ "No bot registered with ID: " ++ show botId
    Just oldPos -> field
      { botsByPosition = Map.insert pos botId $ Map.delete oldPos $ botsByPosition field
      , positionsByBot = Map.insert botId pos $ Map.delete botId $ positionsByBot field
      }

stepBlueBot :: (BotController b, BotController g) => Field b g -> Fryxbot b -> Field b g
stepBlueBot field bot =
  let botId = Bot.id bot
      bot' = Bot.invokeController bot
      field' = updateBlueBot field bot'
  in case (command . Bot.state) bot' of
    Idle        -> field'
    RotateLeft  -> updateBlueBot field' $
                     bot' { Bot.facing = rotateLeft $ Bot.facing bot' }
    RotateRight -> updateBlueBot field' $
                     bot' { Bot.facing = rotateRight $ Bot.facing bot' }
    MoveForward -> let currentPos = lookupBotPos field' botId
                       newPos = adjacent (Bot.facing bot') currentPos
                   in if isBlocked field newPos
                      then field'
                      else setBotPos field' botId newPos
    DropBeacon i -> let currentPos = lookupBotPos field' botId
                        beacons' = Map.insert currentPos (mkBeacon Blue i) (beacons field)
                    in field' { beacons = beacons' }
    DestroyBeacon ->  let currentPos = lookupBotPos field' botId
                          beacons' = Map.delete currentPos (beacons field)
                      in field' { beacons = beacons' }
    PickUpArtifact -> field'
    DropArtifact -> field'

stepGoldBot :: (BotController b, BotController g) => Field b g -> Fryxbot g -> Field b g
stepGoldBot field bot =
  let botId = Bot.id bot
      bot' = (Bot.invokeController bot)
      field' = Field.updateGoldBot field bot'
  in case (command . Bot.state) bot' of
    Idle        -> field'
    RotateLeft  -> updateGoldBot field' $
                     bot' { Bot.facing = rotateLeft $ Bot.facing bot' }
    RotateRight -> updateGoldBot field' $
                     bot' { Bot.facing = rotateRight $ Bot.facing bot' }
    MoveForward -> let currentPos = Field.lookupBotPos field' botId
                       newPos = adjacent (Bot.facing bot') currentPos
                   in if isBlocked field newPos
                      then field'
                      else setBotPos field' botId newPos
    DropBeacon i -> let currentPos = lookupBotPos field' botId
                        beacons' = Map.insert currentPos (mkBeacon Gold i) (beacons field)
                    in field' { beacons = beacons' }
    DestroyBeacon ->  let currentPos = lookupBotPos field' botId
                          beacons' = Map.delete currentPos (beacons field)
                      in field' { beacons = beacons' }
    PickUpArtifact -> field'
    DropArtifact -> field'

cellKind :: (BotController b, BotController g) => Field b g -> Pos -> CellKind
cellKind field pos =
  if Set.member pos (buildings field) then Building
  else if isBlueBotAt field pos then BlueBot blueFacing
  else if isGoldBotAt field pos then GoldBot goldFacing
  else if Map.member pos (artifacts field) then
    Artifacts $ fromJust $ Map.lookup pos (artifacts field)
  else if Set.member pos (blueBase field) then BlueBase
  else if Set.member pos (goldBase field) then GoldBase
  else if Map.member pos (beacons field) then
    let (Beacon team kind) = fromJust $ Map.lookup pos (beacons field)
    in if team == Blue then BlueBeacon kind else GoldBeacon kind
  else Open
    where
      blueFacing =
        case Map.lookup pos (botsByPosition field) of
          Nothing -> error $ "No bot at position " ++ show pos
          Just botId -> case Map.lookup botId (blueBotsById field) of
                          Nothing -> error $ "No blue bot with id " ++ show botId
                          Just bot -> Bot.facing bot
      goldFacing =
        case Map.lookup pos (botsByPosition field) of
          Nothing -> error $ "No bot at position " ++ show pos
          Just botId -> case Map.lookup botId (goldBotsById field) of
                          Nothing -> error $ "No gold bot with id " ++ show botId
                          Just bot -> Bot.facing bot

showCell :: (BotController b, BotController g) => Field b g -> Pos -> Char
showCell field pos = case cellKind field pos of
  Building     -> '#'
  BlueBase     -> 'b'
  GoldBase     -> 'g'
  BlueBot _    -> 'B'
  GoldBot _    -> 'G'
  BlueBeacon _ -> '+'
  GoldBeacon _ -> '-'
  Artifacts i  -> if i > 9 then '*' else intToDigit i
  Open         -> '.'

showRow :: (BotController b, BotController g) => Field b g -> Int -> String
showRow field row =
  let cols = [0..width field]
      rowChars = map (\col -> showCell field $ mkPos col row) cols
      rowString = intersperse ' ' rowChars
  in if row `mod` 2 == 1 then ' ':rowString else rowString

showField :: (BotController b, BotController g) => Field b g -> String
showField field =
  let rows = [0..height field]
  in intercalate "\n" $ map (showRow field) rows
