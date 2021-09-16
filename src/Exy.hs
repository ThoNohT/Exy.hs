module Exy (run) where

import Control.Monad.State.Lazy
import System.IO
import Prelude hiding (until)

data Output = Continue | Quite

run :: IO ()
run = run' 0
  where
    run' st = do
      (out, st') <- runStateT step st
      if out == "quit" then pure () else run' st'

step :: StateT Int IO String
step = do
  liftIO $ putStrLn "Type something..."
  x <- liftIO getLine
  y <- get
  liftIO $ print y
  put $ y + 1
  pure x
