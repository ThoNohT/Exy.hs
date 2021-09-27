module Core (fst3) where

-- | Gets the first element from a 3-tuple.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

