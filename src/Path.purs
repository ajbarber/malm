module Path  where

import Prelude

import Data.Foldable (foldl, foldr)
import Data.Int (floor, toNumber)
import Data.List (snoc)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, fst)
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Graph (LNode, LPath(..))
import GraphRep (fromNode)
import Types (Direction(..), DirectionTick(..), Path(..), Coords)

toPath ::
  Number ->
  LPath Int ->
  Path DirectionTick
toPath xMax (LP { unLPath: arr}) = _.path $ foldr (\a b -> {
                                              path: toPathSegment xMax b.path b.prev a,
                                              prev: Just a }) { prev: Nothing, path: End } arr

toPathSegment ::
  Number ->
  Path DirectionTick ->
  Maybe (LNode Int) ->
  LNode Int ->
  Path DirectionTick
toPathSegment xMax path (Just prev) cur = toDirectionChange (x0 /\ y0) (x1 /\ y1) path
  where
    (x0 /\ y0) = fromNode xMax (fst prev)
    (x1 /\ y1) = fromNode xMax (fst cur)
toPathSegment _ _ _ _ = End

toDirectionChange ::
  Tuple Number Number ->
  Tuple Number Number ->
  (Path DirectionTick -> Path DirectionTick)
toDirectionChange (x0 /\ y0) (x1 /\ y1) =
  if (x1 - x0) < 0.0 then
    Path (DirectionTick Right 1.0) (x0 - x1)
  else if (x1 - x0) > 0.0 then
    Path (DirectionTick Left 1.0) (x1 - x0)
  else if (y1 - y0) < 0.0 then
    Path (DirectionTick Down 1.0) (y0 - y1)
  else if (y1 - y0) > 0.0 then
    Path (DirectionTick Up 1.0) (y1 - y0)
  else Path (DirectionTick None 0.0) 0.0

-- Given two coords, calculate a path between them
simplePath :: Coords -> Coords -> Path DirectionTick -> Path DirectionTick
simplePath from to path = case (p1 /\ p2) of
  (Just p1 /\ Just p2) -> p1 (p2 path)
  (Just p1 /\ Nothing) -> p1 path
  (Nothing /\ Just p2) -> p2 path
  _ -> path
  where
    p1 = startAdjust from.xpos to.xpos true
    p2 = startAdjust from.ypos to.ypos false

-- the path finding only finds optimum paths between
-- whole 16x16 grid spaces. We need to get to the nearest
-- 16 x 16 vertex first from wherever we are
appendStartPoint :: Coords -> Path DirectionTick -> Path DirectionTick
appendStartPoint coords path = simplePath coords_ coords path
  where
    coords_ = coords { xpos = 16.0 * toNumber (floor (coords.xpos/16.0)),
                       ypos = 16.0 * toNumber (floor (coords.ypos/16.0))}

-- ensures we don't get any zero movements added to path
startAdjust :: Number -> Number -> Boolean -> Maybe (Path DirectionTick -> Path DirectionTick)
startAdjust x0 x1 isX = case (x1 - x0 == 0.0) of
  true -> Nothing
  false -> Just $ if isX then
             toDirectionChange (x0 /\ 0.0) (x1 /\ 0.0)
           else
             toDirectionChange (0.0 /\ x0) (0.0 /\ x1)
