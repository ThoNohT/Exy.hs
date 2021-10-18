module Parser (Parser (..), end, entire, statement) where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Arrow ((>>>))
import Control.Monad ((>=>))
import Core (fst3)
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set (fromList, member)
import qualified Data.Text as T
import Exy (Expression (..), Operator (..), Primitive (..), Statement (..), Variable (Variable))
import qualified GHC.TypeLits as T
import Lexer (LexInfo (..), Token (..))
import Text.Printf (printf)

-- ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### --

newtype Parser a = Parser {runParser :: [LexInfo Token] -> Either T.Text (a, [LexInfo Token])}

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
  Parser pa >>= f = Parser $ pa >=> (\(a, rest1) -> runParser (f a) rest1)

-- ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### --

-- | A parser that consumes a token, and fails if there is no token left.
token :: Parser (LexInfo Token)
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

-- | Like check, but runs the check on the token in a LexInfo, and transforms only the token, not the info around it.
checkTkn :: (a -> Either T.Text b) -> Parser (LexInfo a) -> Parser (LexInfo b)
checkTkn checkFn =
  check
    ( \v ->
        case checkFn $ tkn v of
          Right v' -> Right $ LexInfo {tkn = v', whitespaceBefore = whitespaceBefore v}
          Left err -> Left err
    )

numberToken :: Parser (LexInfo Integer)
numberToken =
  token
    & checkTkn
      ( \case
          NumberToken n -> Right n
          _ -> Left "Expected number token."
      )

wordToken :: Parser (LexInfo T.Text)
wordToken =
  token
    & checkTkn
      ( \case
          WordToken n -> Right n
          _ -> Left "Expected word token."
      )

operatorToken :: Parser (LexInfo T.Text)
operatorToken =
  token
    & checkTkn
      ( \case
          OperatorToken op -> Right op
          _ -> Left "Expected operator token."
      )

-- | A parser that succeeds only if there is no more input to consume.
end :: Parser ()
end = Parser $ \case
  [] -> Right ((), [])
  _ -> Left "Expected end of input."

-- | Takes a parser and adds a check that after the parser, there should be no more input.
entire :: Parser a -> Parser a
entire p = p <* end

operator :: Parser Operator
operator =
  operatorToken
    & check
      ( \v -> case tkn v of
          "+" -> Right Plus
          "-" -> Right Minus
          op -> Left $ T.pack $ printf "Invalid operator token: '%s'." op
      )

primitive :: Parser Primitive
primitive = Number <$> (tkn <$> numberToken <|> ((*) <$> minusOp <*> noSpaceNumberToken))
  where
    noSpaceNumberToken =
      numberToken
        & check
          ( \v ->
              if whitespaceBefore v
                then Left "Negative number cannot have whitespace between number an '-' symbol."
                else Right $ tkn v
          )

    minusOp =
      operatorToken
        & check
          ( \v -> case tkn v of
              "-" -> Right (-1 :: Integer)
              "+" -> Right 1
              _ -> Left "Only minus or plus operator can be prefixed to a number"
          )

keywords :: Set T.Text
keywords = Set.fromList ["store", "load", "clear"]

keyword :: Parser T.Text
keyword = wordToken & check (\w -> if Set.member (tkn w) keywords then Right (tkn w) else Left "Word is not a keyword")

variable :: Parser Variable
variable =
  wordToken
    & check
      ( \w ->
          if Set.member (tkn w) keywords
            then Left $ T.pack $ printf "Keyword '%s' cannot be a variable." $ tkn w
            else Right $ Variable $ tkn w
      )

pKeyword :: T.Text -> Parser T.Text
pKeyword name =
  keyword
    & check
      ( \w ->
          if w == name
            then Right w
            else Left $ T.pack $ printf "Expected '%s' keyword but got '%s'." name w
      )

expression :: Parser Expression
expression = binary <|> expr <|> var
  where
    binary = BinaryExpression <$> operator <*> expression <*> expression
    expr = PrimitiveExpression <$> primitive
    var = VariableReference <$> variable

statement :: Parser Statement
statement = store <|> load <|> clear
  where
    load = Load <$> (pKeyword "load" *> variable)
    clear = Clear <$> (pKeyword "clear" *> variable)
    store = Store <$> (pKeyword "store" *> variable) <*> expression
