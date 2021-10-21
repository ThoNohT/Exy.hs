module Runner (run) where

import Control.Monad (foldM_)
import Control.Monad.State.Lazy (MonadIO (liftIO), MonadState (get, put), StateT (runStateT))
import Data.Map.Strict as Map
import qualified Data.Text as T
import Exy (Declaration (..), ExyState, Output (..), Statement (..), createDeclaration, showExpr, Variable (Variable), Expression)
import Lexer (lexText)
import Parser (Parser, end, runParser, statement)
import Text.Printf (printf)
import Control.Monad.RWS.Lazy (modify)

-- | Runs the Exy update loop.
run :: IO ()
run = run' Map.empty
  where
    run' st = do
      (out, st') <- runStateT step st
      if out == Quit then pure () else run' st'

-- | Displays all information about a variable, given the provided state.
showVar :: Variable -> ExyState -> IO ()
showVar var state = do
  putStrLn $ printf "===== [ %s ] =====" (showExpr var)
  let val = Map.lookup var state
  case val of
    Nothing -> liftIO $ putStrLn $ printf "Variable declaration '%s' not found" (show var)
    Just decl -> do
      liftIO $ putStrLn $ printf "Expression: %s" (showExpr (declExpr decl))
      liftIO $ case declType decl of
        Left err -> putStrLn $ printf "Type Error: %s" err
        Right t -> putStrLn $ printf "Type: %s" $ show t

-- | Stores a variable with an expression in the state.
-- Overrides any older declaration for this variable.
storeVar :: Variable -> Expression -> ExyState -> ExyState
storeVar var expr state = do
  let decl = createDeclaration state expr
  Map.insert var decl state

-- | Removes a variable from the state, has no effect if the variable did not exist.
clearVar :: Variable -> ExyState -> ExyState
clearVar = Map.delete

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
          get >>= liftIO . showVar var
        Store var expr -> do
          state <- get
          let newState = storeVar var expr state
          put newState
          liftIO $ showVar var newState
        Clear var -> do
          modify (clearVar var)
          liftIO $ putStrLn "Cleared"
    Left err -> liftIO $ putStrLn $ T.unpack err

  case input of
    "quit" -> pure Quit
    _ -> pure Continue

