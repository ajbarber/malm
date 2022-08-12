module Graph  where

import Prelude

import Data.Foldable (foldl)
import Data.List (List(..), filter, head, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Data.Tuple.Nested (T2, T3, T4, (/\))
import Tuple (over2', over4')

-- | Unlabeled node
type  Node   = Int
-- | Labeled node
type LNode a = T2 Node a
-- | Quasi-unlabeled node
type UNode   = LNode Unit

-- | Unlabeled edge
type  Edge   = T2 Node Node
-- | Labeled edge
type LEdge b = T3 Node Node b
-- | Quasi-unlabeled edge
type UEdge   = LEdge Unit

-- | Unlabeled path
type Path    = List Node
-- | Labeled path
newtype LPath a = LP { unLPath :: List (LNode a) }

instance showLP :: (Show a) => Show (LPath a) where
  show (LP xs) = show xs

instance eqLP :: (Eq a) => Eq (LPath a) where
  eq (LP { unLPath: Nil}) (LP { unLPath: Nil}) = true
  eq (LP { unLPath: x }) (LP { unLPath: y}) = (snd <$> head x) == (snd <$> head y)

instance (Ord a) => Ord (LPath a) where
  compare (LP { unLPath: Nil}) (LP { unLPath: Nil}) = EQ
  compare (LP { unLPath: x}) (LP { unLPath: y}) = compare (snd <$> head x) (snd <$> head y)

-- | Quasi-unlabeled path
type UPath   = List UNode

-- | Labeled links to or from a 'Node'.
type Adj b        = List (T2 b Node)
-- | Links to the 'Node', the 'Node' itself, a label, links from the 'Node'.
--
--   In other words, this captures all information regarding the
--   specified 'Node' within a graph.
type Context a b  = T4 (Adj b) Node a (Adj b) -- Context a b "=" Context' a b "+" Node
type MContext a b = Maybe (Context a b)
-- | 'Graph' decomposition - the context removed from a 'Graph', and the rest
-- of the 'Graph'.
type Decomp g a b = T2 (MContext a b) (g a b)
-- | The same as 'Decomp', only more sure of itself.
type GDecomp g a b  = T2 (Context a b) (g a b)

-- | Unlabeled context.
type UContext     = T3 (List Node) Node (List Node)
-- | Unlabeled decomposition.
type UDecomp g    = T2 (Maybe UContext) g

class Graph gr where

  -- | An empty 'Graph'.
  empty :: forall a b. gr a b

  -- | True if the given 'Graph' is empty.
  isEmpty :: forall a b. gr a b -> Boolean

  -- | Decompose a 'Graph' into the 'MContext' found for the given node and the
  -- remaining 'Graph'.
  match :: forall a b. Node -> gr a b -> Decomp gr a b

  -- | Create a 'Graph' from the list of 'LNode's and 'LEdge's.
  --
  mkGraph :: forall a b. List (LNode a) -> List (LEdge b) -> gr a b

  -- | A list of all 'LNode's in the 'Graph'.
  labNodes :: forall a b. gr a b -> List (LNode a)

  -- | Decompose a graph into the 'Context' for an arbitrarily-chosen 'Node'
  -- and the remaining 'Graph'.
  -- matchAny :: forall a b. gr a b -> GDecomp gr a b

  -- | The number of 'Node's in a 'Graph'.
  noNodes   :: forall a b. gr a b -> Int

  -- | The minimum and maximum 'Node' in a 'Graph'.
  -- nodeRange :: forall a b. gr a b -> T2 Node Node

  -- -- | A list of all 'LEdge's in the 'Graph'.
  labEdges  :: forall a b. gr a b -> List (LEdge b)

class Graph gr <= DynGraph gr where
  -- | Merge the 'Context' into the 'DynGraph'.
  --
  --   Context adjacencies should only refer to either a Node already
  --   in a graph or the node in the Context itself (for loops).
  --
  --   Behaviour is undefined if the specified 'Node' already exists
  --   in the graph.
  merge :: forall a b. Context a b -> gr a b -> gr a b

infixr 8 merge as &

-- | Insert a 'LEdge' into the 'Graph'.
insEdge :: forall gr a b. (DynGraph gr) => LEdge b -> gr a b -> gr a b
insEdge (v /\ w /\ l) g = case mcxt of
  Just  mcxt' -> merge (over4' ((l /\ w):_) mcxt') g'
  Nothing -> g
  where
    (mcxt /\ g') = match v g

-- | Insert multiple 'LEdge's into the 'Graph'.
insEdges :: forall gr a b. (DynGraph gr) => List (LEdge b) -> gr a b -> gr a b
insEdges es g = foldl (flip insEdge) g es

context4l' :: forall a b. Context a b -> Adj b
context4l' (p /\ v /\ _ /\ s) = s <> filter ((_==v) <<< snd) p

-- | All 'Node's linked to in a 'Context'.
suc' :: forall a b. Context a b -> List Node
suc' = map snd <<< context4l'

-- | The 'Node' in a 'Context'.
node' :: forall a b. Context a b -> Node
node' (_ /\ v /\ _ /\ _) = v

-- | An empty LPath
emptyLP :: forall a. LPath a
emptyLP = LP { unLPath: Nil }
