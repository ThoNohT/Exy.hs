module Parser
  ( Parser (..),
    runParser',
    mkParser,
    token,
    numberToken,
    wordToken,
    operatorToken,
    bracketToken,
    end,
    operator,
    primitive,
    keyword,
    bracket,
    variable,
    expression,
    statement,
  )
where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Arrow ((>>>))
import Control.Monad ((>=>))
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set (fromList, member)
import Data.Text (toLower)
import qualified Data.Text as T
import Exy (Expression (..), Operator (..), Primitive (..), Statement (..), Variable (Variable))
import qualified GHC.TypeLits as T
import Lexer (LexInfo (..), Token (..))
import Text.Printf (printf)

-- ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### --

data ParseResult i a
  = Failure T.Text
  | CommittedFailure T.Text T.Text
  | Success (a, [i])

instance Functor (ParseResult i) where
  fmap f (Success (r, rest)) = Success (f r, rest)
  fmap f (Failure err) = Failure err
  fmap f (CommittedFailure name err) = CommittedFailure name err

newtype Parser i a = Parser {runParser :: [i] -> ParseResult i a}

runParser' :: Parser i a -> [i] -> Either T.Text (a, [i])
runParser' p i = case runParser p i of
  Success (r, rest) -> Right (r, rest)
  Failure err -> Left err
  CommittedFailure name err -> Left $ T.pack $ printf "%s: %s" name err

-- | Creates a parser from a function that turns the input list into a result and the remaining input.
mkParser :: ([i] -> Either T.Text (a, [i])) -> Parser i a
mkParser f = Parser $ \input ->
  case f input of
    Right (r, rest) -> Success (r, rest)
    Left err -> Failure err

instance Functor (Parser i) where
  fmap f (Parser p) = Parser (fmap f . p)

instance Applicative (Parser i) where
  pure a = Parser $ \x -> Success (a, x)
  Parser pf <*> Parser pb = Parser $ \input ->
    case pf input of
      Success (f, rest1) ->
        case pb rest1 of
          Success (b, rest2) ->
            Success (f b, rest2)
          Failure e -> Failure e
          CommittedFailure name e -> CommittedFailure name e
      Failure e -> Failure e
      CommittedFailure name e -> CommittedFailure name e

instance Alternative (Parser i) where
  empty = Parser $ const $ Failure ""
  Parser pa <|> Parser pb = Parser $ \input -> case pa input of
    Success result -> Success result
    Failure _ -> case pb input of
      Success result -> Success result
      Failure err -> Failure err
      CommittedFailure name err -> CommittedFailure name err
    CommittedFailure name leftErr -> CommittedFailure name leftErr

instance Monad (Parser i) where
  Parser pa >>= f = Parser $ \input -> case pa input of
    Success (r, rest) -> runParser (f r) rest
    Failure err -> Failure err
    CommittedFailure name err -> CommittedFailure name err

-- Transforms a parser into one that is committed and will no longer be alternated.
commit :: T.Text -> Parser i r -> Parser i r
commit n (Parser p) = Parser $ \input ->
  case p input of
    Success s -> Success s
    Failure e -> CommittedFailure n e
    CommittedFailure name e -> CommittedFailure (T.pack $ printf "%s:%s" n name) e

-- ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### --

type TknParser = Parser (LexInfo Token)

-- | A parser that consumes a token, and fails if there is no token left.
token :: TknParser (LexInfo Token)
token = Parser $ \case
  [] -> Failure "No more input"
  tkn : rest -> Success (tkn, rest)

-- | Takes a parser, and runs the check function on its result.
-- The resulting parser fails if either the original parser fails, or if it succeeds but the check function returns an
-- error on the result.
check :: (a -> Either T.Text b) -> TknParser a -> TknParser b
check checkFn p = p >>= checkParser
  where
    checkParser v = Parser $ \input -> case checkFn v of
      Right e -> Success (e, input)
      Left r -> Failure r

-- | Like check, but runs the check on the token in a LexInfo, and transforms only the token, not the info around it.
checkTkn :: (a -> Either T.Text b) -> TknParser (LexInfo a) -> TknParser (LexInfo b)
checkTkn checkFn =
  check
    ( \v ->
        case checkFn $ tkn v of
          Right v' -> Right $ LexInfo {tkn = v', whitespaceBefore = whitespaceBefore v}
          Left err -> Left err
    )

numberToken :: TknParser (LexInfo Integer)
numberToken =
  token
    & checkTkn
      ( \case
          NumberToken n -> Right n
          _ -> Left "Expected number token"
      )

wordToken :: TknParser (LexInfo T.Text)
wordToken =
  token
    & checkTkn
      ( \case
          WordToken n -> Right n
          _ -> Left "Expected word token"
      )

operatorToken :: TknParser (LexInfo T.Text)
operatorToken =
  token
    & checkTkn
      ( \case
          OperatorToken op -> Right op
          _ -> Left "Expected operator token"
      )

bracketToken :: TknParser (LexInfo T.Text)
bracketToken =
  token
    & checkTkn
      ( \case
          BracketToken b -> Right b
          _ -> Left "Expected bracket token"
      )

-- | A parser that succeeds only if there is no more input to consume.
end :: TknParser ()
end = Parser $ \case
  [] -> Success ((), [])
  _ -> Failure "Expected end of input"

operator :: TknParser Operator
operator =
  operatorToken
    & check
      ( \v -> case tkn v of
          "+" -> Right Plus
          "-" -> Right Minus
          "=" -> Right Equals
          "|" -> Right Or
          "&" -> Right And
          op -> Left $ T.pack $ printf "Invalid operator token: '%s'" op
      )

primitive :: TknParser Primitive
primitive = Number <$> number <|> Truth <$> truth
  where
    number = tkn <$> numberToken <|> ((*) <$> minusOp <*> noSpaceNumberToken)

    truth = True <$ pKeyword "yes" <|> False <$ pKeyword "no"

    noSpaceNumberToken =
      numberToken
        & check
          ( \v ->
              if whitespaceBefore v
                then Left "Negative number cannot have whitespace between number an '-' symbol"
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
keywords = Set.fromList ["quit", "store", "load", "clear", "yes", "no"]

keyword :: TknParser T.Text
keyword =
  wordToken
    & check
      ( \w ->
          if Set.member (toLower $ tkn w) keywords
            then Right (tkn w)
            else Left $ T.pack $ printf "Word '%s' is not a keyword" (tkn w)
      )

bracket :: T.Text -> TknParser ()
bracket str =
  bracketToken
    & check
      ( \b ->
          if tkn b == str
            then Right ()
            else Left $ T.pack $ printf "Expected bracket '%s' but got '%s'" str (tkn b)
      )

variable :: TknParser Variable
variable =
  wordToken
    & check
      ( \w ->
          if Set.member (tkn w) keywords
            then Left $ T.pack $ printf "Keyword '%s' cannot be a variable" $ tkn w
            else Right $ Variable $ tkn w
      )

pKeyword :: T.Text -> TknParser T.Text
pKeyword name =
  keyword
    & check
      ( \w ->
          if toLower w == toLower name
            then Right w
            else Left $ T.pack $ printf "Expected '%s' keyword but got '%s'" name w
      )

expression :: Bool -> TknParser Expression
expression descend = if descend then binary <|> nonDescending else nonDescending
  where
    nonDescending = grouped <|> prim <|> var
    -- Expressions can only expand on the right side, making the operators left-associative
    -- Eg. "1 + 2 + 3" --> can only be parsed to 1 + (2 + 3) since the left cannot descend.
    binary = flip BinaryExpression <$> expression False <*> operator <*> expression True
    grouped = GroupedExpression <$> (bracket "(" *> expression True <* bracket ")")
    prim = PrimitiveExpression <$> primitive
    var = VariableReference <$> variable

statement :: TknParser Statement
statement = quit <|> store <|> load <|> clear
  where
    quit = Quit <$ pKeyword "quit"
    load = Load <$> (pKeyword "load" *> commit "load" variable)
    clear = Clear <$> (pKeyword "clear" *> commit "clear" variable)
    store = Store <$> (pKeyword "store" *> commit "store" variable) <*> commit "store expression" (expression True)
