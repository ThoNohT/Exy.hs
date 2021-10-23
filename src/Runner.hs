module Runner (run) where

import Control.Monad (foldM_)
import Control.Monad.RWS.Lazy (modify)
import Control.Monad.State.Lazy (MonadIO (liftIO), MonadState (get, put), StateT (runStateT))
import Data.Map.Strict as Map
import Data.Set as Set
import qualified Data.Text as T
import Exy
  ( Declaration (..),
    Expression,
    ExyState,
    Output (..),
    Statement (..),
    Variable (Variable),
    allDependencies,
    clearDeclaration,
    createDeclaration,
    dependencies,
    recalculateState,
    setDependencies,
    showExpr,
  )
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

-- | Displays all information about a variable, given the provided state.
showVar :: Variable -> ExyState -> IO ()
showVar var state = do
  putStrLn $ printf "===== [ %s ] =====" (showExpr var)
  let val = Map.lookup var state
  case val of
    Nothing -> liftIO $ putStrLn "Variable declaration not found"
    Just decl@DeclaredDeclaration {} -> do
      putStrLn $ printf "Expression: %s" (showExpr (declExpr decl))
      case declType decl of
        Left err -> putStrLn $ printf "Type Error: %s" err
        Right t -> putStrLn $ printf "Type: %s" $ show t
      liftIO $ putStrLn "Dependents:"
      liftIO $ print $ declDependents decl
    Just decl@UndeclaredDeclaration {} -> do
      putStrLn "Undeclared declaration."
      putStrLn "Dependents:"
      print $ declDependents decl

-- | Stores a variable with an expression in the state, and recalculates the state to update all declarations
-- depending on this declaration.
-- Overrides any older declaration for this variable.
storeVar :: Variable -> Expression -> ExyState -> IO ExyState
storeVar var expr state =
  case allDependencies state expr of
    deps | not $ Set.member var deps -> do
      -- Ok, we can insert.
      pure $
        recalculateState (Set.singleton var) $
          Map.insert var (createDeclaration var state expr) $
            setDependencies var (dependencies expr) state
    _ -> do
      putStrLn $ printf "Storing expression: %s would introduce a circular dependency." (showExpr expr)
      pure state

-- | Clears the declaration with the specified variable from the state, and recalculates the state to update all
-- declarations depending on this declaration.
clearVar :: Variable -> ExyState -> ExyState
clearVar var state = recalculateState (Set.singleton var) $ clearDeclaration var $ setDependencies var Set.empty state

-- | A single step in the Exy update loop.
step :: StateT ExyState IO Output
step = do
  liftIO $ putStrLn "Type something..."
  input <- T.pack <$> liftIO getLine

  let parsed = runParser (statement <* end) =<< lexText input

  case parsed of
    Right (stmt, _) -> do
      case stmt of
        Load var ->
          get >>= liftIO . showVar var
        Store var expr -> do
          state <- get
          newState <- liftIO $ storeVar var expr state
          put newState
          liftIO $ showVar var newState
        Clear var -> do
          modify (clearVar var)
          liftIO $ putStrLn "Cleared"
    Left err -> liftIO $ putStrLn $ T.unpack err

  case input of
    "quit" -> pure Quit
    _ -> pure Continue
