{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Fryxbots.Field
  ( CellKind(..)
  , Field(..)
  , addBlueBot
  , addGoldBot
  , blueScore
  , cellKind
  , deleteBotById
  , getBlueBots
  , getFossilCount
  , getGoldBots
  , goldScore
  , isBlocked
  , isBlueBase
  , isGoldBase
  , lookupBotPos
  , lookupBotTeam
  , mkField
  , numBlueBots
  , numGoldBots
  , scanHex
  , setBotPos
  , setBlueBase
  , setBuilding
  , setFossils
  , setGoldBase
  , showField
  , updateBlueBot
  , updateGoldBot
  ) where

import           Data.Char (intToDigit)
import           Data.List (intercalate, intersperse)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Fryxbots.Beacon
import           Fryxbots.Bot (Bot)
import qualified Fryxbots.Bot as Bot
import           Fryxbots.Bot.Controller
import           Fryxbots.Bot.Facing
import qualified Fryxbots.Bot.Sensing as Sense
import           Fryxbots.Pos
import           Fryxbots.Team

data Field b g where
  Field :: forall b g. (Controller b, Controller g) =>
           { width          :: Int
           , height         :: Int
           , blueBotsById   :: Map Int (Bot b)
           , goldBotsById   :: Map Int (Bot g)
           , botsByPosition :: Map Pos Int
           , positionsByBot :: Map Int Pos
           , fossils        :: Map Pos Int
           , buildings      :: Set Pos
           , blueBase       :: Set Pos
           , goldBase       :: Set Pos
           , blueBeacons    :: Map Pos Beacon
           , goldBeacons    :: Map Pos Beacon
           } -> Field b g

mkField :: (Controller b, Controller g) => Int -> Int -> Field b g
mkField width height = Field
  { width = width
  , height = height
  , blueBotsById = Map.empty
  , goldBotsById = Map.empty
  , botsByPosition = Map.empty
  , positionsByBot = Map.empty
  , fossils = Map.empty
  , buildings = Set.empty
  , blueBase = Set.empty
  , goldBase = Set.empty
  , blueBeacons = Map.empty
  , goldBeacons = Map.empty
  }

data CellKind = Building
              | BlueBase
              | GoldBase
              | BlueBot Facing
              | GoldBot Facing
              | BlueBeacon BeaconKind
              | GoldBeacon BeaconKind
              | Fossils Int
              | Open

setBuilding :: (Controller b, Controller g) =>
               Field b g -> Pos -> Field b g
setBuilding field pos = field {
  buildings = Set.insert pos $ buildings field
}

setBlueBase :: (Controller b, Controller g) =>
               Field b g -> Pos -> Field b g
setBlueBase field pos = field {
  blueBase = Set.insert pos $ blueBase field
}

setGoldBase :: (Controller b, Controller g) =>
               Field b g -> Pos -> Field b g
setGoldBase field pos = field {
  goldBase = Set.insert pos $ goldBase field
}

blueScore :: (Controller b, Controller g) => Field b g -> Int
blueScore field =
  let blueFossils = map (getFossilCount field) $ Set.toList (blueBase field)
  in sum blueFossils

goldScore :: (Controller b, Controller g) => Field b g -> Int
goldScore field =
  let goldFossils = map (getFossilCount field) $ Set.toList (goldBase field)
  in sum goldFossils

getFossilCount :: (Controller b, Controller g) =>
              Field b g -> Pos -> Int
getFossilCount field pos =
  case Map.lookup pos $ fossils field of
    Nothing -> 0
    Just i  -> i

setFossils :: (Controller b, Controller g) =>
              Field b g -> Pos -> Int -> Field b g
setFossils field pos i =
  case i of
    0 -> field { fossils = Map.delete pos $ fossils field }
    _ -> field { fossils = Map.insert pos i $ fossils field }

isBlueBase :: (Controller b, Controller g) =>
              Field b g -> Pos -> Bool
isBlueBase field pos = Set.member pos $ blueBase field

isGoldBase :: (Controller b, Controller g) =>
              Field b g -> Pos -> Bool
isGoldBase field pos = Set.member pos $ goldBase field

isBase :: (Controller b, Controller g) =>
          Field b g -> Team -> Pos -> Bool
isBase field team pos = case team of
                    Blue -> isBlueBase field pos
                    Gold -> isGoldBase field pos

blueBeaconAt :: (Controller b, Controller g) =>
                Field b g -> Pos -> Maybe BeaconKind
blueBeaconAt field pos = do
  beacon <- Map.lookup pos $ blueBeacons field
  return $ kind beacon

goldBeaconAt :: (Controller b, Controller g) =>
                Field b g -> Pos -> Maybe BeaconKind
goldBeaconAt field pos = do
  beacon <- Map.lookup pos $ goldBeacons field
  return $ kind beacon

beaconAt :: (Controller b, Controller g) =>
                Field b g -> Team -> Pos -> Maybe BeaconKind
beaconAt field team pos = case team of
                            Blue -> blueBeaconAt field pos
                            Gold -> goldBeaconAt field pos

addBlueBot :: (Controller b, Controller g) =>
              Field b g -> Bot b -> Pos -> Field b g
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

addGoldBot :: (Controller b, Controller g) =>
               Field b g -> Bot g -> Pos -> Field b g
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

numBlueBots :: (Controller b, Controller g) => Field b g -> Int
numBlueBots = Map.size . blueBotsById

numGoldBots :: (Controller b, Controller g) => Field b g -> Int
numGoldBots = Map.size . goldBotsById

deleteBlueBotById :: (Controller b, Controller g) =>
                      Field b g -> Int -> Field b g
deleteBlueBotById field botId =
  let pos = lookupBotPos field botId
  in field
    { blueBotsById   = Map.delete botId $ blueBotsById   field
    , botsByPosition = Map.delete pos   $ botsByPosition field
    , positionsByBot = Map.delete botId $ positionsByBot field
    }

deleteGoldBotById :: (Controller b, Controller g) =>
                 Field b g -> Int -> Field b g
deleteGoldBotById field botId =
  let pos = lookupBotPos field botId
  in field
    { goldBotsById   = Map.delete botId $ goldBotsById   field
    , botsByPosition = Map.delete pos   $ botsByPosition field
    , positionsByBot = Map.delete botId $ positionsByBot field
    }

deleteBotById :: (Controller b, Controller g) =>
             Field b g -> Int -> Field b g
deleteBotById field botId = case lookupBotTeam field botId of
      Blue -> deleteBlueBotById field botId
      Gold -> deleteGoldBotById field botId

lookupBotTeam :: (Controller b, Controller g) =>
                  Field b g -> Int -> Team
lookupBotTeam field botId =
  case Map.lookup botId (blueBotsById field) of
    Just bot -> Bot.team bot
    Nothing  -> case Map.lookup botId (goldBotsById field) of
                  Just bot -> Bot.team bot
                  Nothing  -> error $ "No bot with id: " ++ (show botId)

updateBlueBot :: (Controller b, Controller g) =>
                  Field b g -> Bot b -> Field b g
updateBlueBot field bot =
  let botId = Bot.id bot
      botsMap = blueBotsById field
  in if Map.member botId botsMap
     then field { blueBotsById = Map.insert botId bot botsMap }
     else error $ "No bot registered with ID: " ++ show botId

updateGoldBot :: (Controller b, Controller g) =>
                  Field b g -> Bot g -> Field b g
updateGoldBot field bot =
  let botId = Bot.id bot
  in case Map.lookup botId (goldBotsById field) of
    Nothing  -> error $ "No bot registered with ID: " ++ show botId
    Just _ -> field { goldBotsById = Map.insert botId bot $ goldBotsById field }

getBlueBots :: (Controller b, Controller g) => Field b g -> [Bot b]
getBlueBots field = Map.elems $ blueBotsById field

getGoldBots :: (Controller b, Controller g) => Field b g -> [Bot g]
getGoldBots field = Map.elems $ goldBotsById field

lookupBotPos :: (Controller b, Controller g) => Field b g -> Int -> Pos
lookupBotPos field botId = case Map.lookup botId $ positionsByBot field of
  Nothing  -> error $ "No bot registered with ID: " ++ show botId
  Just pos -> pos

isBlueBotAt :: (Controller b, Controller g) => Field b g -> Pos -> Bool
isBlueBotAt field pos = case Map.lookup pos (botsByPosition field) of
  Nothing -> False
  Just botId -> Map.member botId (blueBotsById field)

isGoldBotAt :: (Controller b, Controller g) => Field b g -> Pos -> Bool
isGoldBotAt field pos = case Map.lookup pos (botsByPosition field) of
  Nothing -> False
  Just botId -> Map.member botId (goldBotsById field)

isBotAt :: (Controller b, Controller g) => Field b g -> Pos -> Team -> Bool
isBotAt field pos team =
  case team of
    Blue -> isBlueBotAt field pos
    Gold -> isGoldBotAt field pos

isBuildingAt :: (Controller b, Controller g) =>
                Field b g -> Pos -> Bool
isBuildingAt field pos = Set.member pos (buildings field)

isBlocked :: (Controller b, Controller g) =>
             Field b g -> Pos -> Bool
isBlocked field pos = or $ map ($ pos) [isBuildingAt field, isBlueBotAt field, isGoldBotAt field]

setBotPos :: (Controller b, Controller g) =>
             Field b g -> Int -> Pos -> Field b g
setBotPos field botId pos =
  case Map.lookup botId (positionsByBot field) of
    Nothing -> error $ "No bot registered with ID: " ++ show botId
    Just oldPos -> field
      { botsByPosition = Map.insert pos botId $ Map.delete oldPos $ botsByPosition field
      , positionsByBot = Map.insert botId pos $ Map.delete botId $ positionsByBot field
      }

containsPos :: (Controller b, Controller g) =>
               Field b g -> Pos -> Bool
containsPos field pos = posX pos >= 0 && posX pos < width field
                     && posY pos >= 0 && posY pos < height field

scanHex :: (Controller b, Controller g) =>
           Field b g -> Pos -> Sense.HexScan
scanHex field pos =
  if (not . containsPos field) pos
    then Sense.OffMap
    else Sense.HexScan
        { Sense.beacon = \team -> beaconAt field team pos
        , Sense.isBuilding = isBuildingAt field pos
        , Sense.isBase = \team -> isBase field team pos
        , Sense.hasBot = isBotAt field pos
        , Sense.numFossils = case Map.lookup pos (fossils field) of
                              Nothing -> 0
                              Just n  -> n
        }

cellKind :: (Controller b, Controller g) =>
            Field b g -> Pos -> CellKind
cellKind field pos =
  if Set.member pos (buildings field) then Building
  else if isBlueBotAt field pos then BlueBot blueFacing
  else if isGoldBotAt field pos then GoldBot goldFacing
  else if Map.member pos (fossils field) then
    Fossils $ fromJust $ Map.lookup pos (fossils field)
  else if Set.member pos (blueBase field) then BlueBase
  else if Set.member pos (goldBase field) then GoldBase
  else if Map.member pos (blueBeacons field) then
       let (Beacon _ kind) = fromJust $ Map.lookup pos (blueBeacons field)
       in BlueBeacon kind
  else if Map.member pos (goldBeacons field) then
       let (Beacon _ kind) = fromJust $ Map.lookup pos (goldBeacons field)
       in GoldBeacon kind
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

showCell :: (Controller b, Controller g) =>
            Field b g -> Pos -> Char
showCell field pos = case cellKind field pos of
  Building     -> '#'
  BlueBase     -> 'b'
  GoldBase     -> 'g'
  BlueBot _    -> 'B'
  GoldBot _    -> 'G'
  BlueBeacon _ -> '+'
  GoldBeacon _ -> '-'
  Fossils i    -> if i > 9 then '*' else intToDigit i
  Open         -> '.'

showRow :: (Controller b, Controller g) =>
           Field b g -> Int -> String
showRow field row =
  let cols = [0..width field]
      rowChars = map (\col -> showCell field $ mkPos col row) cols
      rowString = intersperse ' ' rowChars
  in if row `mod` 2 == 1 then ' ':rowString else rowString

showField :: (Controller b, Controller g) =>
             Field b g -> String
showField field =
  let rows = [0..height field]
  in intercalate "\n" $ map (showRow field) rows
