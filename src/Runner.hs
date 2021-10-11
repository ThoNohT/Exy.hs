module Runner (run) where

import Control.Monad.State.Lazy (StateT (runStateT), MonadIO (liftIO), MonadState (get, put))
import Exy (ExyState, Output(..))
import qualified Data.Text as T
import Lexer (lexText)
import Parser (expression, runParser, end)

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
  input <- T.pack <$> liftIO getLine
  let lexed = lexText input
  let parsed =
        case lexed of
          Right tkns -> runParser (expression <* end) tkns
          _ -> Left "No tokens to parse."

  liftIO $ print lexed
  liftIO $ print parsed

  case parsed of
    Right (res, _) -> do
      oldState <- get
      put $ res : oldState
      pure ()
    _ -> pure ()

  newState <- get
  liftIO $ print newState
  case input of
    "quit" -> pure Quit
    _ -> pure Continue
