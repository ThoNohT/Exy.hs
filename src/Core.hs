module Core (fst3, filterMaybe) where

-- | Gets the first element from a 3-tuple.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | Checks a predicate on a maybe value, and if it is false, changes the Maybe to Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe predicate maybe = maybe >>= (\v -> if predicate v then Just v else Nothing)
