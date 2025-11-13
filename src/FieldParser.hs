{-# LANGUAGE OverloadedStrings #-}

module FieldParser
  ( parseField
  ) where

import           Data.Char (digitToInt, isDigit)
import           Data.Text (Text)
import           Data.Void
import           Field
import qualified Pos as Pos
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseField :: (BotController b, BotController g) => Text -> IO (Either String (Field b g))
parseField fieldStr = do
  case parse fieldParser "" fieldStr of
    Left bundle -> return $ Left (errorBundlePretty bundle)
    Right field -> return $ Right field

fieldParser :: (BotController b, BotController g) => Parser (Field b g)
fieldParser = do
  width <- parseInt
  space1
  height <- parseInt
  newline
  items <- count height (fieldRow width)
  return $
    foldl (\field (row, itemRow) ->
      foldl (\field (col, item) -> handleCellKind field row col item)
        field
        (zip [0..] itemRow))
    (mkField width height) (zip [0..] items)

handleCellKind :: (BotController b, BotController g) => Field b g -> Int -> Int -> CellKind -> Field b g
handleCellKind field row col item =
  let pos = Pos.mkPos col row
  in case item of
    Building -> setBuilding field pos
    BlueBase -> setBlueBase field pos
    GoldBase -> setGoldBase field pos
    Fossils i -> setFossils field pos i
    _ -> field

fieldRow :: Int -> Parser [CellKind]
fieldRow width = do
  row <- count width itemParser
  newline
  return row

itemParser :: Parser CellKind
itemParser = do
  space
  c <- anySingle
  case c of
    '#' -> return Building
    '+' -> return BlueBase
    '-' -> return GoldBase
    _ -> return $ if isDigit c
                    then Fossils (digitToInt c)
                    else Open

parseInt :: Parser Int
parseInt = L.decimal
