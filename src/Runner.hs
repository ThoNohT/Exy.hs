module Runner (run) where

import Control.Monad.State.Lazy (StateT (runStateT), MonadIO (liftIO), MonadState (get, put))
import Exy (ExyState, Output(..), Statement (..))
import qualified Data.Text as T
import Lexer (lexText)
import Parser (Parser, statement, runParser, end)
import Data.Map.Strict as Map

-- | Runs the Exy update loop.
run :: IO ()
run = run' Map.empty
  where
    run' st = do
      (out, st') <- runStateT step st
      if out == Quit then pure () else run' st'

-- | A single step in the Exy update loop.
step :: StateT ExyState IO Output
step = do
  liftIO $ putStrLn "Type something..."
  input <- T.pack <$> liftIO getLine

  let parsed = runParser (statement <* end) =<< lexText input

  case parsed of
    Right (stmt, _) -> do
      case stmt of
        Load var -> do
          oldState <- get
          let val = Map.lookup var oldState
          case val of
            Nothing -> liftIO $ putStrLn "Variable not found"
            Just expr -> liftIO $ print expr
        Store var expr -> do
          oldState <- get
          put $ Map.insert var expr oldState
    Left err -> liftIO $ putStrLn $ T.unpack err

  case input of
    "quit" -> pure Quit
    _ -> pure Continue
