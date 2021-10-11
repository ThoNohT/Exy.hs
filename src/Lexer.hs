module Lexer (Token (..), outputFormat, LexInfo(..), lexText) where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad ((>=>))
import Core (fst3)
import Data.Bifunctor (first)
import Data.Char (isDigit, isLetter, isSpace)
import Data.Functor ((<&>))
import qualified Data.Text as T
import GHC.Exts (the)
import System.Directory (doesFileExist)
import Text.ParserCombinators.ReadP (satisfy)
import Text.Printf (printf)

-- ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### --

data Token
  = NumberToken Integer
  | OperatorToken T.Text
  deriving (Show, Eq, Ord)

outputFormat :: Token -> T.Text
outputFormat (NumberToken n) = T.pack $ printf "number '%i'" n
outputFormat (OperatorToken n) = T.pack $ printf "operator '%s'" n

-- | Returns the specified text, or Nothing if it is empty.
notEmpty :: T.Text -> Maybe T.Text
notEmpty "" = Nothing
notEmpty x = Just x

-- ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### --

newtype Lexer a = Lexer {runLexer :: T.Text -> Maybe (a, T.Text)}

instance Functor Lexer where
  fmap f (Lexer l) = Lexer (fmap (first f) . l)

instance Applicative Lexer where
  pure a = Lexer $ \x -> Just (a, x)
  Lexer lf <*> Lexer lv = Lexer $ lf >=> (\(f, rest1) -> fmap (first f) (lv rest1))

instance Alternative Lexer where
  empty = Lexer $ const Nothing
  Lexer la <|> Lexer lb = Lexer $ \input -> la input <|> lb input

instance Monad Lexer where
  Lexer la >>= f = Lexer $ la >=> (\ (a, rest1) -> runLexer (f a) rest1)

-- ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### --

whitespace :: Lexer T.Text
whitespace = Lexer $ \input -> consumeResult id input <$> notEmpty (T.takeWhile isSpace input)

consumeResult :: (T.Text -> a) -> T.Text -> T.Text -> (a, T.Text)
consumeResult f input result = (f result, T.drop (T.length result) input)

number :: Lexer Integer
number = Lexer $ \input ->
  case T.uncons input of
    Nothing -> Nothing
    Just ('-', rest) -> first ((-1) *) <$> runLexer positiveNumber rest
    _ -> runLexer positiveNumber input

positiveNumber :: Lexer Integer
positiveNumber = Lexer $ \input -> consumeResult (read . T.unpack) input <$> notEmpty (T.takeWhile isDigit input)

operator :: Lexer T.Text
operator = Lexer $ \input ->
  case T.uncons input of
    Just ('-', rest) -> Just ("-", rest)
    Just ('+', rest) -> Just ("+", rest)
    _ -> Nothing

token :: Lexer Token
token = fmap NumberToken positiveNumber <|> fmap OperatorToken operator

-- ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### --

data LexInfo a = LexInfo { tkn :: a, whitespaceBefore :: Bool } deriving  Show

lexText :: T.Text -> Either T.Text [LexInfo Token]
lexText input =
  let
    wrapToken :: Bool -> Token -> LexInfo Token
    wrapToken hasWs tkn = LexInfo { tkn = tkn , whitespaceBefore = hasWs }
  in
  case (input, runLexer (whitespace *> token) input, runLexer token input) of
    ("", _, _) -> Right []
    (_, Just (res, rest), _) -> fmap (wrapToken True res :) (lexText rest)
    (_, Nothing, Just (res, rest)) -> fmap (wrapToken False res :) (lexText rest)
    (rest, Nothing, Nothing) -> Left $ T.pack $ printf "Failed lexing with remaining input: '%s'" rest
