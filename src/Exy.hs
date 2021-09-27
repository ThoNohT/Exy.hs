module Exy (run, Parser (..), tokenP, wordP, numP, parseText, Location (..), TokenData (..), Token (..), lexFile) where

import Control.Monad.State.Lazy
import Data.Char (isDigit, isLetter, isSpace)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.IO
import Text.Printf (printf)
import Prelude hiding (until)
import Data.Text.Internal.Lazy (Text)

-- | Gets the first element from a 3-tuple.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- Lexer

data Location = Location {row :: Integer, col :: Integer, file :: T.Text} deriving (Eq, Ord)

instance Show Location where
  show (Location row col file) = printf "%s:%d:%d" file row col

nextCol loc = loc {col = col loc + 1}

addCols cols loc = loc {col = col loc + cols}

nextRow loc = loc {row = row loc + 1, col = 0}

data TokenData = WordToken T.Text | StringToken T.Text | NumberToken Integer deriving (Show, Eq, Ord)

outputFormat :: TokenData -> T.Text
outputFormat (WordToken w) = T.pack $ printf "word '%s'" w
outputFormat (StringToken t) = T.pack $ printf "string '%s'" t
outputFormat (NumberToken n) = T.pack $ printf "number '%i'" n

isNumber :: TokenData -> Bool
isNumber = \case
  NumberToken _ -> True
  _ -> False

isWord :: T.Text -> TokenData -> Bool
isWord name = \case
  WordToken n | n == name -> True
  _ -> False

data Token = Token {location :: Location, token :: TokenData} deriving (Eq, Ord)

instance Show Token where
  show (Token loc token) = printf "%s: %s" (show loc) (outputFormat token)

-- | Consumes characters until the first non-whitespace character.
--
-- Also updates the location to point to where the first non-whitespace character is located. If no non-whitespace
-- character is found until the end of the string, Nothing is returned.
consumeWhitespace :: (Location, T.Text) -> Maybe (Location, T.Text)
consumeWhitespace (loc, input) =
  T.uncons input >>= \(c, rem) ->
    case c of
      '\n' -> consumeWhitespace (nextRow loc, rem)
      _ | isSpace c -> consumeWhitespace (nextCol loc, rem)
      _ -> Just (loc, input)

-- | Consumes a token.
--
-- Returns the parsed token, the location after the token where lexing can continue, and the remaining text.
consumeToken :: (Location, T.Text) -> (Token, Location, T.Text)
consumeToken (loc, input) =
  let wordChars = T.takeWhile isLetter input
      numChars = T.takeWhile isDigit input
      nonWordChars = T.takeWhile (\c -> not (isLetter c) && not (isDigit c) && not (isSpace c)) input

      convertResult token text = (token, addCols (toInteger $ T.length text) loc, T.drop (T.length text) input)
   in if not $ T.null wordChars
        then convertResult (Token loc (WordToken wordChars)) wordChars
        else
          if not $ T.null numChars
            then convertResult (Token loc (NumberToken (read $ T.unpack numChars))) numChars
            else
              if not $ T.null nonWordChars
                then convertResult (Token loc (WordToken nonWordChars)) nonWordChars
                else error "None of the recognizable tokens matched."

lexTokens :: Location -> [(Token, Location, T.Text)] -> T.Text -> [(Token, Location, T.Text)]
lexTokens loc acc input =
  case consumeWhitespace (loc, input) <&> consumeToken of
    Nothing -> acc
    Just (next, loc, rem) -> lexTokens loc ((next, loc, rem) : acc) rem

lexFile :: T.Text -> IO (Either T.Text [Token])
lexFile fileName = do
  exists <- doesFileExist (T.unpack fileName)
  if not exists
    then pure $ Left "File does not exist"
    else do
      contents <- T.pack <$> readFile (T.unpack fileName)
      pure $ Right $ reverse $ fst3 <$> lexTokens (Location 0 0 fileName) [] contents

-- End lexer

-- Parser

newtype Parser a = Parser {runParser :: [Token] -> Either (Location, T.Text) (a, [Token])}

parseText :: Parser a -> T.Text -> Either (Location, T.Text) (a, [Token])
parseText p txt = runParser p $ reverse $ fst3 <$> lexTokens (Location 0 0 "~") [] txt

-- | A parser that consumes the next token, regardless of what it is. Fails if there is no token left to consume.
-- TODO: Is this function needed?
tokenP :: Parser Token
tokenP = Parser $ \case
  [] -> Left (Location 0 0 "", "Expected a token, but got nothing.")
  x : xs -> Right (x, xs)

-- | A parser that consumes a word token with the specified word, and fails otherwise.
wordP :: T.Text -> Parser Token
wordP name = Parser $ \case
  [] -> Left (Location 0 0 "", T.pack $ printf "Expected word '%s', but got nothing." name)
  x : xs | (isWord name . token) x -> Right (x, xs)
  x : _ -> Left (location x, T.pack $ printf "Expected word '%s' but got %s" name (outputFormat $ token x))

numP :: Parser Token
numP = Parser $ \case
  [] -> Left (Location 0 0 "", "Expected a number token, but got nothing.")
  x : xs | (isNumber . token) x -> Right (x, xs)
  x : _ -> Left (location x, T.pack $ printf "Expected number token but got %s" (outputFormat $ token x))




-- End parser

-- Types

data Output = Continue | Quit deriving (Eq)

type ExyState = [T.Text]

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
  x <- T.pack <$> liftIO getLine
  y <- get
  put $ x : y
  y <- get
  liftIO $ print y
  case x of
    "quit" -> pure Quit
    _ -> pure Continue

-- End program
