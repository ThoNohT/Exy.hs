module Exy (run) where

import Control.Monad.State.Lazy
import System.IO
import Text.Parsec (alphaNum, anyChar, char, digit, letter, many1, (<|>), string)
import Text.Parsec.Text.Lazy (Parser)
import Prelude hiding (until)

newtype Variable = MkVariable String

data Literal = NumLiteral Integer | StringLiteral String

-- | A statement that can be typed into the command line.
data Statement
  = Set Variable Literal
  | Get Variable

variableParser :: Parser Variable
variableParser = do
  firstChar <- letter
  otherChars <- many1 variableChar
  pure $ MkVariable (firstChar : otherChars)
  where
    variableChar = alphaNum <|> char '_'

numLiteral = do
  nums <- many1 digit
  pure $ NumLiteral $ read nums

stringLiteral = do
  chars <- char '"' *> many1 anyChar <* char '\"'
  pure $ StringLiteral chars

literalParser :: Parser Literal
literalParser = numLiteral <|> stringLiteral


setStatementParser :: Parser Statement
setStatementParser = do
  _ <- string "set"
  var <- variableParser
  Set var <$> literalParser

getStatementParser :: Parser Statement
getStatementParser = do
  _ <- string "get"
  Get <$> variableParser

statementParser :: Parser Statement
statementParser = getStatementParser <|> setStatementParser

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
