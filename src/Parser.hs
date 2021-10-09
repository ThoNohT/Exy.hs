module Parser () where

import Core (fst3)
import qualified Data.Text as T
import Text.Printf (printf)
import qualified Exy

--newtype Parser a = Parser {runParser :: [Token] -> Either (Location, T.Text) (a, [Token])}

--parseText :: Parser a -> T.Text -> Either (Location, T.Text) (a, [Token])
--parseText p txt = runParser p $ reverse $ fst3 <$> lexTokens (Location 0 0 "~") [] txt

-- | A parser that consumes the next token, regardless of what it is. Fails if there is no token left to consume.
-- TODO: Is this function needed?
--tokenP :: Parser Token
--tokenP = Parser $ \case
 -- [] -> Left (Location 0 0 "", "Expected a token, but got nothing.")
 -- x : xs -> Right (x, xs)

-- | A parser that consumes a word token with the specified word, and fails otherwise.
--wordP :: T.Text -> Parser Token
--wordP name = Parser $ \case
--  [] -> Left (Location 0 0 "", T.pack $ printf "Expected word '%s', but got nothing." name)
--  x : xs | (isWord name . token) x -> Right (x, xs)
--  x : _ -> Left (location x, T.pack $ printf "Expected word '%s' but got %s" name (outputFormat $ token x))
--
--numP :: Parser Token
--numP = Parser $ \case
--  [] -> Left (Location 0 0 "", "Expected a number token, but got nothing.")
--  x : xs | (isNumber . token) x -> Right (x, xs)
--  x : _ -> Left (location x, T.pack $ printf "Expected number token but got %s" (outputFormat $ token x))


-- Domain parsers

--parseVariable :: Parser Exy.Variable
--parseVariable = undefined
