module Core (filterMaybe) where

import Data.Set (Set)
import qualified Data.Set as Set

-- | Checks a predicate on a maybe value, and if it is false, changes the Maybe to Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe predicate maybe = maybe >>= (\v -> if predicate v then Just v else Nothing)
