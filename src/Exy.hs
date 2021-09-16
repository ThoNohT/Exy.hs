module Exy (run) where

import Control.Monad.State.Lazy
import System.IO
import Prelude hiding (until)

data Output = Continue | Quit deriving (Eq)

run :: IO ()
run = run' 0
  where
    run' st = do
      (out, st') <- runStateT step st
      if out == Quit then pure () else run' st'

step :: StateT Int IO Output
step = do
  liftIO $ putStrLn "Type something..."
  x <- liftIO getLine
  y <- get
  liftIO $ print y
  put $ y + 1
  case x of
    "quit" -> pure Quit
    _ -> pure Continue
