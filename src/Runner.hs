module Runner (run) where

import Control.Monad (foldM_)
import Control.Monad.State.Lazy (MonadIO (liftIO), MonadState (get, put), StateT (runStateT))
import Data.Map.Strict as Map
import qualified Data.Text as T
import Exy (Declaration (..), ExyState, Output (..), Statement (..), createDeclaration, showExpr)
import Lexer (lexText)
import Parser (Parser, end, runParser, statement)
import Text.Printf (printf)

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
            Nothing -> liftIO $ putStrLn $ printf "Variable declaration '%s' not found" (show var)
            Just decl -> do
              liftIO $ putStrLn $ printf "Expression: %s" (showExpr (declExpr decl))
              liftIO $ foldM_ (\_ e -> putStrLn $ printf "Error: %s" e) () (declErrors decl)
              liftIO $ foldM_ (\_ t -> putStrLn $ printf "Type: %s" $ show t) () (declType decl)
        Store var expr -> do
          oldState <- get
          let decl = createDeclaration oldState expr
          put $ Map.insert var decl oldState
          liftIO $ putStrLn "Stored"
        Clear var -> do
          oldState <- get
          put $ Map.delete var oldState
          liftIO $ putStrLn "Cleared"
    Left err -> liftIO $ putStrLn $ T.unpack err

  case input of
    "quit" -> pure Quit
    _ -> pure Continue
