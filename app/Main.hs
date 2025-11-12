{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Brick
import           Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import           Control.Concurrent (threadDelay, forkIO)
import           Control.Monad (forever, void)
import           Data.Char (intToDigit)
import           Data.Text (pack)
import           FieldParser
import           FryxbotSimulator
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes.Color as VC
import qualified Graphics.Vty.CrossPlatform as VCP
import           RandController

data Tick = Tick
type Name = ()

--
-- Modify these definitions to define the controllers
-- used for the blue and gold teams, respectively
--
blueController = mkRandController
goldController = mkRandController
--
--
--

app :: (BotController b, BotController g) => App (Game b g) Tick Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const theMap
  }

main :: IO ()
main = do
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000
  fieldStr <- readFile "worlds/example1.world"
  field <- parseField $ pack fieldStr
  case field of
    Left err -> putStrLn err
    Right field -> do
      let game = mkGame blueController goldController field
      let buildVty = VCP.mkVty V.defaultConfig
      initialVty <- buildVty
      _ <- customMain initialVty buildVty (Just chan) app game
      putStrLn "Simulation complete!"

handleEvent :: (BotController b, BotController g) => BrickEvent Name Tick -> EventM Name (Game b g) ()
handleEvent (AppEvent Tick) = modify executeRound
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc []))        = halt
handleEvent _                                     = return ()

drawUI :: (BotController b, BotController g) => Game b g -> [Widget Name]
drawUI game = [C.center $ drawGrid game]

drawGrid :: (BotController b, BotController g) => Game b g -> Widget Name
drawGrid game = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "THE FRYXELL WARS")
  $ vBox rows
  where
    rows = [hBox $ drawRow row | row <- [0..(height.field) game]]

    drawRow :: Int -> [Widget Name]
    drawRow row =
      let cells = [drawCoord (mkPos col row) | col <- [0..(width.field) game]]
      in if row `mod` 2 == 1 then (str " "):cells else cells

    faceStr :: BotFacing -> String
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
                      BlueBeacon i -> withAttr (attrName "blueBeacon") (intToSmall i)
                      GoldBeacon i -> withAttr (attrName "goldBeacon") (intToSmall i)
                      Fossils i -> withAttr (attrName "cyan") (intToStr i)
                      BlueBot facing -> withAttr (attrName "blueBot") (str $ faceStr facing)
                      GoldBot facing -> withAttr (attrName "goldBot") (str $ faceStr facing)
                      Open -> withAttr (attrName "gray") (str ". ")
        where intToStr i = (str $ if i <= 9 then (intToDigit i):" " else "* ")
              intToSmall i = str $ case i of
                               0 -> "⁰ "
                               1 -> "¹ "
                               2 -> "² "
                               3 -> "³ "
                               4 -> "⁴ "
                               5 -> "⁵ "
                               6 -> "⁶ "
                               7 -> "⁷ "
                               8 -> "⁸ "
                               9 -> "⁹ "
                               _ -> "⁺ "

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
