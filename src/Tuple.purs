module Tuple where

import Data.Tuple.Nested (T2, T4, (/\))

-- | Given at least a 4-tuple, modifies the fourth value.
over4' :: forall a b c d r. (d -> r) -> T4 a b c d -> T4 a b c r
over4' o (a /\ b /\ c /\ d) = a /\ b /\ c /\ o d

over2' :: forall a b r. (b -> r) -> T2 a b -> T2 a r
over2' o (a /\ b) = a /\ o b
