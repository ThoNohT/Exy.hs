module Exy (run) where

import Data.Text
import Control.Monad.State.Lazy
import System.IO
import Prelude hiding (until)

-- Lexer

data Location = Location { row :: Integer, col :: Integer, file :: Text } deriving (Show, Eq, Ord)
data TokenData = WordToken Text | StringToken Text | NumberToken Integer deriving (Show, Eq, Ord)
data Token = Token { location :: Location, token :: TokenData } deriving (Show, Eq, Ord)

lexToken :: Location -> Text -> Either Text (Token, Location)
lexToken loc input = Left "Not implemented..."

lexFile :: Text -> Either Text [ Token ]
lexFile _ = Left "Not implemented"


-- End lexer

-- Types

data Output = Continue | Quit deriving (Eq)

type ExyState = [Text]

-- End types

-- Program

run :: IO ()
run = run' []
  where
    run' st = do
      (out, st') <- runStateT step st
      if out == Quit then pure () else run' st'

step :: StateT ExyState IO Output
step = do
  liftIO $ putStrLn "Type something..."
  x <- pack <$> liftIO getLine
  y <- get
  put $ x : y
  y <- get
  liftIO $ print y
  case x of
    "quit" -> pure Quit
    _ -> pure Continue

-- End program
