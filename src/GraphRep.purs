module GraphRep  where

import Prelude

import Data.Array (concatMap, difference, filter, foldl, range)
import Data.Int (floor, toNumber)
import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Graph (LEdge, LNode, mkGraph)
import Location (collision'')
import Math ((%))
import Tree (Gr)
import Types (LoadedTileA, SpriteState, Coords, dest, toCoords)

curGraphNode ::  SpriteState -> Tuple Int Int
curGraphNode ss =
  Tuple (floor $ (dest ss.location).xpos % 16.0)
        (floor $ (dest ss.location).ypos % 16.0)

mkGraph' :: forall a b. Array (LNode a) -> Array (LEdge b) -> Gr a b
mkGraph' vs es = mkGraph (fromFoldable vs) (fromFoldable es)

toNode :: Number -> Tuple Number Number -> Int
toNode xMax (xpos /\ ypos) = floor (ypos/16.0) * floor (xMax/16.0) + floor (xpos/16.0)

-- toNode transforms to Graph node representation but without snapping to the grid
toNode' :: Number -> Tuple Number Number -> Number
toNode' xMax (xpos /\ ypos) = ypos/16.0 * xMax/16.0 + xpos/16.0

fromNode :: Number -> Int -> Tuple Number Number
fromNode xMax node = let
  y = toNumber $ floor $ (toNumber node*16.0/xMax)
  div = toNumber (node*16) % xMax
  in Tuple div (y * 16.0)

neighbours :: Number -> Coords -> Array Coords -> Array Int
neighbours xMax xy walls =
  toNode xMax <$> (\a -> Tuple a.xpos a.ypos)  <$> filterWalls walls [ xy { xpos = xy.xpos - xy.w },
                                                                       xy { xpos = xy.xpos + xy.w },
                                                                       xy { ypos = xy.ypos - xy.h },
                                                                       xy { ypos = xy.ypos + xy.h } ]

-- toGraph ::forall a.  Number -> Array (LoadedTileA a) -> Array Coords -> Gr Int Int
-- toGraph xMax tiles walls = mkGraph' (vertices xMax tiles) (concatMap (\xy -> (addEdge $ toNode xMax (xy.xpos /\ xy.ypos)) <$> neighbours xMax xy walls) (toCoords <$> tiles))

-- experimental version where we ignore the tiles
toGraph' :: forall a.  Number -> Number -> Array Coords -> Gr Int Int
toGraph' xMax yMax walls = mkGraph' (vertices' xMax yMax walls) (concatMap (\xy -> (addEdge $ toNode xMax (xy.xpos /\ xy.ypos)) <$> neighbours xMax xy walls) $ tiles' xMax yMax walls)

vertices' :: forall a. Number -> Number -> Array Coords -> Array (LNode Int)
vertices' xMax yMax walls = (\n -> (n /\ 1)) <$> (\t -> toNode xMax (t.xpos /\ t.ypos)) <$> tiles' xMax yMax walls

shift :: Number -> Int -> Array Coords
shift xMax y = (\e -> { xpos: 16.0 * toNumber e,
                        ypos: 16.0 * toNumber y,
                        h: 16.0,
                        w: 16.0,
                        perimeter: 0.0,
                        xoffset: 0.0,
                        yoffset: 0.0} ) <$> range 0 (floor(xMax/16.0))

tiles' :: Number -> Number -> Array Coords -> Array Coords
tiles' xMax yMax walls = filterWalls walls (concatMap (shift xMax) $ range 0 (floor(yMax/16.0)))

filterWalls :: Array Coords -> Array Coords -> Array Coords
filterWalls walls arr = foldl (flip removed) arr walls

removed :: Coords -> Array Coords -> Array Coords
removed wall = filter (not (flip collision'') wall)

vertices :: forall a. Number -> Array (LoadedTileA a) -> Array (LNode Int)
vertices xMax tiles = (\n -> (n /\ 1)) <$> (\t -> toNode xMax (t.xpos /\ t.ypos)) <$> toCoords <$> tiles

addEdge :: Int -> Int -> LEdge Int
addEdge src dst = (src /\ dst /\ 1)
