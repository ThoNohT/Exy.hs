module Lexer (Token (..), outputFormat, lexString) where

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

data Token
  = NumberToken Integer
  | OperatorToken String
  deriving (Show, Eq, Ord)

outputFormat :: Token -> T.Text
outputFormat (NumberToken n) = T.pack $ printf "number '%i'" n
outputFormat (OperatorToken n) = T.pack $ printf "operator '%s'" n

-- | Returns the specified text, or Nothing if it is empty.
notEmpty :: T.Text -> Maybe T.Text
notEmpty "" = Nothing
notEmpty x = Just x

isNumber :: Token -> Bool
isNumber = \case
  NumberToken _ -> True
  _ -> False

-- ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### --

newtype Lexer a = Lexer {runLexer :: T.Text -> Maybe (a, T.Text)}

instance Functor Lexer where
  fmap f lexer = Lexer (fmap (first f) . runLexer lexer)

instance Applicative Lexer where
  pure a = Lexer $ \x -> Just (a, x)
  fl <*> vl = Lexer $ runLexer fl >=> (\(f, rest1) -> fmap (first f) (runLexer vl rest1))

instance Alternative Lexer where
  empty = Lexer $ const Nothing
  al <|> bl = Lexer $ \input -> runLexer al input <|> runLexer bl input

instance Monad Lexer where
  al >>= f = Lexer $ runLexer al >=> (\ (a, rest1) -> runLexer (f a) rest1)

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

operator :: Lexer String
operator = Lexer $ \input ->
  case T.uncons input of
    Just ('-', rest) -> Just ("-", rest)
    Just ('+', rest) -> Just ("+", rest)
    _ -> Nothing

token :: Lexer Token
token = fmap NumberToken positiveNumber <|> fmap OperatorToken operator

lexString :: T.Text -> Either String [Token]
lexString input =
  case (input, runLexer token input, runLexer whitespace input) of
    ("", _, _) -> Right []
    (_, Just (res, rest), _) -> fmap (res :) (lexString rest)
    (_, Nothing, Just (_, rest)) -> lexString rest
    (rest, Nothing, Nothing) -> Left $ printf "Failed lexing with remaining input: %s" rest
