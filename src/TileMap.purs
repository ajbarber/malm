module TileMap where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)
import Data.Array (range)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number.Format (toString)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import ExampleMaps (dMap, lMap)
import Hero as Hero
import Image (loadImg)
import Record as Record
import Types (InputCoordMap, Location(..), TileData, TileMap(..), LoadedTile)

loadedTileMap :: Aff (Array LoadedTile)
loadedTileMap = do
  traverse (\t -> do
     img <- loadImg (i t)
     pure { e: img,
            loc: t.loc,
            wall: t.wall}) td
  where
    TileMap basePath td = tileMap
    i t = (basePath <> "/" <> t.file)

formatInt :: Int -> String
formatInt i = case (i < 10) of
  true -> "0" <> asStr i
  false -> asStr i
  where
    asStr = toString <<< toNumber

placeholderMap :: String
placeholderMap = dMap

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
