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
import Parser (Parser, end, runParser', statement)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

-- | Runs the Exy update loop.
run :: IO ()
run = run' Map.empty
  where
    run' st = do
      (continue, st') <- runStateT step st
      if continue then run' st' else pure ()

-- | Displays all information about a variable, given the provided state.
showVar :: Variable -> ExyState -> IO ()
showVar var state = do
  putStrLn $ printf "===== [ %s ] =====" (showExpr var)
  let val = Map.lookup var state
  case val of
    Nothing -> putStrLn "Variable declaration not found"
    Just decl@DeclaredDeclaration {} -> do
      putStrLn $ printf "Expression: %s" (showExpr (declExpr decl))
      case declType decl of
        Left err -> putStrLn $ printf "Type Error: %s" err
        Right t -> putStrLn $ printf "Type: %s" $ show t
      case declValue decl of
        Nothing -> putStrLn "No value known"
        Just v -> putStrLn $ printf "Value: %s" (show v)
      putStrLn $ printf "Dependents: %s" (T.intercalate ", " $ showExpr <$> Set.toList (declDependents decl))
    Just decl@UndeclaredDeclaration {} -> do
      putStrLn "Undeclared declaration."
      putStrLn $ printf "Dependents: %s" (T.intercalate ", " $ showExpr <$> Set.toList (declDependents decl))

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
step :: StateT ExyState IO Bool
step = do
  liftIO $ putStr "Exy> "
  liftIO $ hFlush stdout
  input <- T.pack <$> liftIO getLine

  let parsed = runParser' (statement <* end) =<< lexText input

  case parsed of
    Right (stmt, _) -> do
      case stmt of
        Load var -> do
          get >>= liftIO . showVar var
          pure True
        Store var expr -> do
          state <- get
          newState <- liftIO $ storeVar var expr state
          put newState
          liftIO $ showVar var newState
          pure True
        Clear var -> do
          modify (clearVar var)
          liftIO $ putStrLn "Cleared"
          pure True
        Quit -> pure False
    Left err -> do
      liftIO $ putStrLn $ printf "Parse error: %s" err
      pure True
