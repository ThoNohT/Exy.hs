module Exy (run) where

import Control.Monad.State.Lazy
import System.IO
import Prelude hiding (until)

data Output = Continue | Quit deriving (Eq)

type ExyState = [ String ]

run :: IO ()
run = run' []
  where
    run' st = do
      (out, st') <- runStateT step st
      if out == Quit then pure () else run' st'

step :: StateT ExyState IO Output
step = do
  liftIO $ putStrLn "Type something..."
  x <- liftIO getLine
  y <- get
  put $ x : y
  y <- get
  liftIO $ print y
  case x of
    "quit" -> pure Quit
    _ -> pure Continue
