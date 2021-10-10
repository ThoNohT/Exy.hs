module Parser (Parser(..), end, entire, operator, primitive, expression) where

import Core (fst3)
import qualified Data.Text as T
import Lexer (Token (..))
import Text.Printf (printf)
import Exy (Expression(..), Primitive(..), Operator(..))
import qualified Data.List as List
import Control.Applicative (Alternative, (<|>), empty)
import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Function ((&))

-- ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### --

newtype Parser a = Parser {runParser :: [Token] -> Either T.Text (a, [Token])}

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative Parser where
  pure a = Parser $ \x -> Right (a, x)
  Parser pf <*> Parser pb = Parser $ pf >=> (\(f, rest1) -> fmap (first f) (pb rest1))

instance Alternative Parser where
  empty = Parser $ const $ Left ""
  Parser pa <|> Parser pb = Parser $ \input -> case pa input of
    Right result -> Right result
    Left leftErr -> case pb input of
      Right result -> Right result
      Left _ -> Left leftErr

instance Monad Parser where
  Parser pa >>= f = Parser $ pa >=> (\ (a, rest1) -> runParser (f a) rest1)

-- ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### --

-- | A parser that consumes a token, and fails if there is no token left.
token :: Parser Token
token = Parser $ \case
  [] -> Left "No more input."
  tkn : rest -> Right (tkn, rest)

-- | Takes a parser, and runs the check function on its result.
-- The resulting parser fails if either the original parser fails, or if it succeeds but the check function returns an
-- error on the result.
check :: (a -> Either T.Text b) -> Parser a -> Parser b
check checkFn p = p >>= checkParser
  where
      checkParser v = Parser $ \input -> case checkFn v of
        Right e -> Right (e, input)
        Left r -> Left r

-- | A parser that succeeds only if there is no more input to consume.
end :: Parser ()
end = Parser $ \case
  [] -> Right ((), [])
  _ -> Left "Expected end of input."

-- | Takes a parser and adds a check that after the parser, there should be no more input.
entire :: Parser a -> Parser a
entire p = p <* end

operator :: Parser Operator
operator = token & check (\case
  OperatorToken "+" -> Right Plus
  OperatorToken "-" -> Right Minus
  OperatorToken op -> Left $ T.pack $ printf "Invalid operator token: '%s'" op
  _ -> Left "Not an operator token.")

primitive :: Parser Primitive
primitive = token & check (\case
  NumberToken n -> Right $ Number n
  _ -> Left "Not a primitive token.")

expression :: Parser Expression
expression = flip BinaryExpression <$> primitive <*> operator <*> primitive

