module GraphRep  where

import Prelude

import Data.Array (concatMap)
import Data.Int (floor, toNumber)
import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Graph (LEdge, LNode, mkGraph)
import Math ((%))
import Tree (Gr)
import Types (Coords, LoadedTileA, SpriteState, dest, toCoords)

curGraphNode ::  SpriteState -> Tuple Int Int
curGraphNode ss =
  Tuple (floor $ (dest ss.location).xpos % 16.0)
        (floor $ (dest ss.location).ypos % 16.0)

mkGraph' :: forall a b. Array (LNode a) -> Array (LEdge b) -> Gr a b
mkGraph' vs es = mkGraph (fromFoldable vs) (fromFoldable es)

toNode :: Number -> Tuple Number Number -> Int
toNode xMax (xpos /\ ypos) = floor (ypos/16.0) * floor (xMax/16.0) + floor (xpos/16.0)

fromNode :: Number -> Int -> Tuple Number Number
fromNode xMax node = let
  y = toNumber $ floor $ (toNumber node*16.0/xMax)
  div = toNumber (node*16) % xMax
  in Tuple div (y * 16.0)

neighbours :: Number -> Coords -> Array Int
neighbours xMax xy =
  toNode xMax <$> [ Tuple (xy.xpos - xy.h) xy.ypos,
                    Tuple (xy.xpos + xy.h) xy.ypos,
                    Tuple xy.xpos (xy.ypos + xy.w),
                    Tuple xy.xpos (xy.ypos - xy.w) ]

toGraph ::forall a.  Number -> Array (LoadedTileA a) -> Gr Int Int
toGraph xMax tiles = mkGraph' (vertices xMax tiles) (concatMap (\xy -> (addEdge $ toNode xMax (xy.xpos /\ xy.ypos)) <$> neighbours xMax xy) (toCoords <$> tiles))

vertices :: forall a. Number -> Array (LoadedTileA a) -> Array (LNode Int)
vertices xMax tiles = (\n -> (n /\ 1)) <$> (\t -> toNode xMax (t.xpos /\ t.ypos)) <$> toCoords <$> tiles

addEdge :: Int -> Int -> LEdge Int
addEdge src dst = (src /\ dst /\ 1)
