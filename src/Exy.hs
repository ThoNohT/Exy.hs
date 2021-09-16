module Exy (run) where

import Prelude hiding (until)
import System.IO

until :: Monad m => (a -> Bool) -> m a -> m a
until break m = do
    res <- m
    if break res then pure res else until break m

run :: IO ()
run = do
    _ <- until (== "quit") $ do
            putStrLn "Type something..."
            getLine
    pure ()

