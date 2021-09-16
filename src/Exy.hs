module Exy (run) where

import Prelude hiding (until)

import Control.Monad.State.Lazy
import System.IO

until :: Monad m => (a -> Bool) -> m a -> m a
until break m = do
    res <- m
    if break res then pure res else until break m

run :: IO ()
run = do
    _ <- until ("quit" ==) $ evalStateT step 0
    pure ()


step :: StateT Int IO String
step = do
    liftIO $ putStrLn "Type something..."
    x <- liftIO getLine
    y <- get
    liftIO $ print y
    put $ y + 1
    pure x

