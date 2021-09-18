module Exy (run) where

import Control.Monad.State.Lazy
import System.IO
import Prelude hiding (until)

-- | The different types of tokens available for building commands.
data Token
  = Keyword String
  | Variable Variable
  | Literal Literal

newtype Variable = MkVariable String

data Literal = NumLiteral Integer | StringLiteral String

-- | A statement that can be typed into the command line.
data Statement
  = Set Variable Literal
  | Get Variable

data Output = Continue | Quit deriving (Eq)

type ExyState = [String]

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
