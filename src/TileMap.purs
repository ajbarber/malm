module TileMap where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)
import Data.Array (filter, fold, last, range, sort, tail)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Number.Format (toString)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import ExampleMaps (dMap, lMap)
import Hero as Hero
import Image (loadImg)
import Record as Record
import Types (InputCoordMap, LoadedTile, Location(..), TileData, TileMap(..), LoadedTileMap, dest)

loadedTileMap :: Aff LoadedTileMap
loadedTileMap = do
  arr <- loadedTiles
  pure { tiles: arr,
         xMin: 0.0,
         yMin: -Hero.height/2.0,
         xMax: xMax tileData + Hero.width,
         yMax: yMax tileData - Hero.height/2.0,
         walls: filter _.wall arr
       }

loadedTiles :: Aff (Array LoadedTile)
loadedTiles = do
  traverse (\t -> do
     img <- loadImg (i t)
     pure { img: img,
            location: t.loc,
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

xMax :: Array TileData -> Number
xMax arr = fromMaybe 0.0 $ last $ sort $ map (\e -> (dest e.loc).xpos) arr

yMax :: Array TileData -> Number
yMax arr = fromMaybe 0.0 $ last $ sort $ map (\e -> (dest e.loc).ypos) arr
