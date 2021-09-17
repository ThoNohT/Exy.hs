module Exy (run) where

import Control.Monad.State.Lazy
import System.IO
import Prelude hiding (until)

-- Type alias for a function that can parse a value of type a.
-- Returns either an error string, or the parsed value combined with the remaining string.
type Parser a = String -> Either String (a, String)

-- Type class for a type that can be parsed.
class Parse a where
  parse :: Parser a

pChar :: Parser Char
pChar [] = Left "No more input"
pChar (x : xs) = Right (x, xs)

pCheck :: (a -> Bool) -> Parser a -> Parser a
pCheck check parser input =
  let pResult = parser input
   in case pResult of
        Left err -> Left err
        Right (p, rem) -> if check p then pResult else Left "Check on parse result failed"

data Statement
  = Set String String
  | Get String

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
