module TileMap where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)
import Data.Array (range)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Number.Format (toString)
import ExampleMaps (lindsayMap)
import Hero as Hero
import Record as Record
import Types (Location(..), TileData, TileMap(..), InputCoordMap)

formatInt :: Int -> String
formatInt i = case (i < 10) of
  true -> "0" <> asStr i
  false -> asStr i
  where
    asStr = toString <<< toNumber

placeholderMap :: String
placeholderMap = lindsayMap

tileMapIn :: Either JsonDecodeError (Array InputCoordMap)
tileMapIn = decodeJson =<< parseJson  placeholderMap

toTileData :: Either JsonDecodeError (Array InputCoordMap) -> Array TileData
toTileData icm = case icm of
  Left e -> []
  Right arr -> map (\x -> {
    file: "Overworld.png",
    loc: Record.merge Hero.baseOffset <$> Location x.src x.dest,
    wall: x.wall }) arr

tileMap :: TileMap
tileMap = TileMap "assets" tileData

tileData :: Array TileData
tileData = toTileData tileMapIn
