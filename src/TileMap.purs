
module TileMap where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)
import Data.Array (filter, find, fold, head, last, range, sort, sortBy, sortWith, tail)
import Data.Either (Either(..))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe, fromMaybe)
import Data.Number.Format (toString)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import ExampleMaps (dMap, lMap)
import Hero as Hero
import Image (loadImg)
import Location (collision')
import Math ((%))
import Record as Record
import Types (Coords, InputCoordMap, LoadedTile, LoadedTileMap, Location(..), TileData, TileMap(..), dest, toCoords)

baseOffset :: { xoffset :: Number, yoffset :: Number, perimeter :: Number }
baseOffset = { xoffset: 0.0, yoffset: 0.0, perimeter: 0.0}

loadedTileMap :: Aff LoadedTileMap
loadedTileMap = do
  arr <- loadedTiles
  let (xMin /\ xMax) = minMax (_.xpos) tileData
      (yMin /\ yMax) = minMax (_.ypos) tileData
  pure { tiles: arr,
         xMin,
         yMin,
         xMax,
         yMax,
         walls: dest <$> (_.location) <$> (filter _.wall arr)
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
    loc: Record.merge baseOffset <$> Location x.src x.dest,
    wall: x.wall }) arr

tileMap :: TileMap
tileMap = TileMap "assets" tileData

tileData :: Array TileData
tileData = toTileData tileMapIn

minMax :: (Coords -> Number) -> Array TileData -> Tuple Number Number
minMax f arr = Tuple (fromMaybe 0.0 $ head sorted) (fromMaybe 0.0 $ last sorted)
 where
   sorted = sort $ map (\e -> f <<< dest $ e.loc) arr

findTile :: Array TileData -> Coords -> Maybe TileData
findTile tiles coords = find (\a -> flip collision' (dest a.loc) coords) tiles

tileDistToGraph :: Number -> Number -> Int
tileDistToGraph range pos = if pos == 0.0 then 0 else floor $ range/pos % 16.0
