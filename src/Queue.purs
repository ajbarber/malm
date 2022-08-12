module Queue where

import Prelude

import Data.Function (flip)
import Data.List (List(..), foldl, null, reverse, singleton, (:))
import Data.Tuple (Tuple(..))

data Queue a = MkQueue (List a) (List a)

mkQueue :: forall a. Queue a
mkQueue = MkQueue Nil Nil

queuePut :: forall a. a -> Queue a -> Queue a
queuePut item (MkQueue ins outs) = MkQueue (item:ins) outs

queuePutList :: forall a. List a -> Queue a -> Queue a
queuePutList xs q = foldl (flip queuePut) q xs

queueGet :: forall a. Queue a -> Tuple a (Queue a)
queueGet (MkQueue ins (item:rest)) = Tuple item (MkQueue ins rest)
queueGet (MkQueue ins Nil) = queueGet (MkQueue Nil (reverse ins))

queueEmpty :: forall a. Queue a -> Boolean
queueEmpty (MkQueue ins outs) = null ins && null outs
