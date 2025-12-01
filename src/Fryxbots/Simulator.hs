{-# LANGUAGE OverloadedStrings #-}

module Fryxbots.Simulator (runSimulator) where

import           Brick
import           Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import           Control.Concurrent (threadDelay, forkIO)
import           Control.Concurrent.STM.TVar
import           Control.Monad (forever, void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.STM (atomically)
import           Data.Char (intToDigit)
import           Data.Text (pack)
import           Fryxbots.Beacon
import           Fryxbots.Bot.Controller
import           Fryxbots.Bot.Facing
import           Fryxbots.Field hiding (blueScore, goldScore)
import           Fryxbots.FieldParser
import           Fryxbots.Game
import           Fryxbots.Pos
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes.Color as VC
import qualified Graphics.Vty.CrossPlatform as VCP

data Tick = Tick
type Name = ()

data SimulatorState b g = SimulatorState
  { game :: Game b g
  , tickDelay :: TVar Int
  }

app :: (Controller b, Controller g) => App (SimulatorState b g) Tick Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const theMap
  }

runSimulator :: (Controller b, Controller g) => String -> b -> g -> IO ()
runSimulator worldFile blueController goldController = do
  chan  <- newBChan 10
  tickDelay <- atomically $ newTVar 100000
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    delay <- atomically $ readTVar tickDelay
    threadDelay delay
  fieldStr <- readFile worldFile
  parsedField <- parseField $ pack fieldStr
  case parsedField of
    Left err -> putStrLn err
    Right field -> do
      let simState = SimulatorState { game = mkGame blueController goldController field
                                    , tickDelay = tickDelay
                                    }
      let buildVty = VCP.mkVty V.defaultConfig
      initialVty <- buildVty
      finalState <- customMain initialVty buildVty (Just chan) app simState

      let blue = (blueScore . game) finalState
      let gold = (goldScore . game) finalState
      putStrLn $ (showField . Fryxbots.Game.field . game) finalState
      putStrLn "Simulation complete!"
      putStrLn $ "Blue: " ++ show blue
      putStrLn $ "Gold: " ++ show gold
      putStrLn $ if blue > gold then "Blue wins!"
                 else if gold > blue then "Gold wins!"
                 else "It's a tie!"

handleEvent :: (Controller b, Controller g) => BrickEvent Name Tick -> EventM Name (SimulatorState b g) ()
handleEvent (AppEvent Tick) = do
  modify $ \st -> st { game = (executeRound . game) st }
  st <- get
  if (gameOver . game) st then halt else return ()
handleEvent (VtyEvent (V.EvKey (V.KChar '-') [])) = changeSpeed SlowDown
handleEvent (VtyEvent (V.EvKey (V.KChar '_') [])) = changeSpeed SlowDown
handleEvent (VtyEvent (V.EvKey (V.KChar '=') [])) = changeSpeed SpeedUp
handleEvent (VtyEvent (V.EvKey (V.KChar '+') [])) = changeSpeed SpeedUp
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc []))        = halt
handleEvent _                                     = return ()

data SpeedChange = SpeedUp | SlowDown

changeSpeed :: SpeedChange -> EventM Name (SimulatorState b g) ()
changeSpeed change = do
  simState <- get
  curTickDelay <- liftIO $ atomically $ readTVar (tickDelay simState)
  case change of
    SpeedUp ->
      let newTickDelay = curTickDelay - 10000
      in if newTickDelay > 0
        then do
          liftIO $ atomically $ writeTVar (tickDelay simState) newTickDelay
          return ()
        else do
          let curGame = game simState
          let curJump = roundJump curGame
          let game' = curGame { roundJump = curJump + 1 }
          modify $ \st -> st { game = game' }
    SlowDown ->
      let curJump = roundJump $ game simState
      in if curJump > 1
        then do
          let curGame = game simState
          let newJump = max 1 $ curJump - 5
          let game' = curGame { roundJump = newJump }
          modify $ \st -> st { game = game' }
        else do
          let newTickDelay = curTickDelay + 10000
          liftIO $ atomically $ writeTVar (tickDelay simState) newTickDelay
          return ()

drawUI :: (Controller b, Controller g) => SimulatorState b g -> [Widget Name]
drawUI simState =
  [ C.center $ vBox [ viewport () Vertical $ drawGrid (game simState)
                    , vLimit 1 $ hBox [ padLeft (Pad 3) $ str "q: Quit"
                           , padLeft (Pad 3) $ str "(-/+): Change Speed"
                           , fill ' '
                           , str "Blue: "
                           , str $ (show . blueScore . game) simState
                           , withAttr (attrName "wallColor") (str " | ")
                           , str "Gold: "
                           , str $ (show . goldScore . game) simState
                           , withAttr (attrName "wallColor") (str " | ")
                           , padRight (Pad 7) $ str $ "Round: " ++ (show . roundNum . game) simState
                           ]
                    ]
  ]

drawGrid :: (Controller b, Controller g) => Game b g -> Widget Name
drawGrid game = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "THE FRYXELL WARS")
  $ vBox rows
  where
    rows = [ hBox $ drawRow row | row <- [0..(height.field) game - 1] ]

    drawRow :: Int -> [Widget Name]
    drawRow row =
      let cells = [ drawCoord (mkPos col row) | col <- [0..(width.field) game - 1] ]
      in if row `mod` 2 == 1 then (str " "):cells else cells

    faceStr :: Facing -> String
    faceStr facing = case facing of
                       NorthEast -> "🡽 "
                       East -> "🡺 "
                       SouthEast -> "🡾 "
                       SouthWest -> "🡿 "
                       West -> "🡸 "
                       NorthWest -> "🡼 "

    drawCoord :: Pos -> Widget Name
    drawCoord pos = case cellKind (field game) pos of
                      Building -> withAttr (attrName "wallColor") (str "# ")
                      BlueBase -> withAttr (attrName "blueBase") (str "b ")
                      GoldBase -> withAttr (attrName "goldBase") (str "g ")
                      BlueBeacon kind -> withAttr (attrName "blueBeacon") (drawBeacon kind)
                      GoldBeacon kind -> withAttr (attrName "goldBeacon") (drawBeacon kind)
                      Fossils i -> withAttr (attrName "cyan")
                                   (str $ if i <= 9 then (intToDigit i):" " else "* ")
                      BlueBot facing -> withAttr (attrName "blueBot") (str $ faceStr facing)
                      GoldBot facing -> withAttr (attrName "goldBot") (str $ faceStr facing)
                      Open -> withAttr (attrName "gray") (str ". ")
        where drawBeacon kind = str $ case kind of
                  Kind1 -> "¹ "
                  Kind2 -> "² "
                  Kind3 -> "³ "
                  Kind4 -> "⁴ "
                  Kind5 -> "⁵ "
                  Kind6 -> "⁶ "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (attrName "wallColor", fg $ VC.rgbColor 190 40 30)
  , (attrName "blueBot", fg VC.blue)
  , (attrName "goldBot", fg VC.yellow)
  , (attrName "goldBase", fg $ VC.rgbColor 140 130 50)
  , (attrName "blueBase", fg $ VC.rgbColor 50 60 170)
  , (attrName "goldBeacon", fg $ VC.rgbColor 130 120 40)
  , (attrName "blueBeacon", fg $ VC.rgbColor 35 45 100)
  , (attrName "brown", fg VC.cyan)
  , (attrName "gray", fg $ VC.rgbColor 122 122 122)
  ]
