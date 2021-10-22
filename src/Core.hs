module Core (fst3, filterMaybe, setFromMaybe) where

import Data.Set (Set)
import qualified Data.Set as Set

-- | Gets the first element from a 3-tuple.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | Checks a predicate on a maybe value, and if it is false, changes the Maybe to Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe predicate maybe = maybe >>= (\v -> if predicate v then Just v else Nothing)

-- | Creates a set from a Maybe, either a singleton set if Just, or empty set if Nothing.
setFromMaybe :: forall a. Ord a => Maybe a -> Set a
setFromMaybe Nothing = Set.empty
setFromMaybe (Just a) = Set.singleton a
