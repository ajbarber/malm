module Heap where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (T3, (/\))

data Heap a b = Empty | Node a b (List (Heap a b))

empty :: forall a b. Heap a b
empty = Empty

unit :: forall a b. a -> b -> Heap a b
unit key val = Node key val Nil

insert :: forall a b. Ord a => Tuple a b -> Heap a b -> Heap a b
insert (key /\ val) h = merge (unit key val) h

mergeAll :: forall a b. Ord a => List (Heap a b) -> Heap a b
mergeAll (h1:h2:rest) = merge (merge h1 h2) (mergeAll rest)
mergeAll (Cons h Nil) = h
mergeAll Nil = empty

merge :: forall a b. Ord a => Heap a b -> Heap a b -> Heap a b
merge h Empty = h
merge Empty h = h
merge h@(Node key1 val1 hs) h'@(Node key2 val2 hs') =
  if key1 < key2 then
    Node key1 val1 (h':hs)
  else
    Node key2 val2 (h:hs')

findMin :: forall a b. Heap a b -> Maybe (Tuple a b)
findMin Empty = Nothing
findMin (Node key val _) = Just (key /\ val)

splitMin :: forall a b. (Ord a) => Heap a b -> Maybe (T3 a b (Heap a b))
splitMin Empty             = Nothing
splitMin (Node key val hs) = Just (key /\ val /\ mergeAll hs)

deleteMin :: forall a b. (Ord a) => Heap a b -> Heap a b
deleteMin Empty = Empty
deleteMin (Node _ _ hs) = mergeAll hs

isEmpty :: forall a b. Heap a b -> Boolean
isEmpty Empty = true
isEmpty _     = false
