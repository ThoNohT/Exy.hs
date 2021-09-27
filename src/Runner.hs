module Runner (run) where

import Control.Monad.State.Lazy (StateT (runStateT), MonadIO (liftIO), MonadState (get, put))
import Exy (ExyState, Output(..))
import qualified Data.Text as T

-- | Runs the Exy update loop.
run :: IO ()
run = run' []
  where
    run' st = do
      (out, st') <- runStateT step st
      if out == Quit then pure () else run' st'

-- | A single step in the Exy update loop.
step :: StateT ExyState IO Output
step = do
  liftIO $ putStrLn "Type something..."
  x <- T.pack <$> liftIO getLine
  y <- get
  put $ x : y
  y <- get
  liftIO $ print y
  case x of
    "quit" -> pure Quit
    _ -> pure Continue
