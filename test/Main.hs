module Main (main) where

import qualified Data.Char as C
import qualified Data.List as List
import Exy
import qualified Lexer as L
import qualified Parser as P
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [parserGroup]

parserGroup = testGroup "Parser tests" [parserTypeGroup, parserClassGroup, tokenParsersGroup, expressionParsersGroup]

failParser :: P.Parser () ()
failParser = P.mkParser (const $ Left "Failed")

successParser :: P.Parser () ()
successParser = P.mkParser (const $ Right ((), []))

parserTypeGroup =
  testGroup
    "Tests for the Parser type"
    [ testCase "A parser that always fails" $ P.runParser' failParser [] @?= Left "Failed",
      testCase "A parser that allways succeeds" $ P.runParser' successParser [] @?= Right ((), [])
    ]

pChar = P.mkParser $ \input -> case List.uncons input of
  Just (c, rest) -> Right (c, rest)
  _ -> Left "No more input"

lexInfo :: a -> L.LexInfo a
lexInfo tkn = L.LexInfo tkn False

parserClassGroup =
  testGroup
    "Tests for the typeclass instances of Parser"
    [ testCase "Functor for Parser, <$>" $ P.runParser' (C.ord <$> pChar) "AB" @?= Right (65, "B"),
      testCase "Applicative for Parser, pure" $ P.runParser' (pure 'x') "AB" @?= Right ('x', "AB"),
      testCase "Applicative for Parser, <*>" $
        P.runParser' (P.mkParser (\a -> Right (C.ord, a)) <*> pChar) "AB" @?= Right (65, "B"),
      testCase "Monad for Parser, =<<" $
        P.runParser' ((\a -> P.mkParser (\i -> Right (C.ord a, i))) =<< pChar) "AB" @?= Right (65, "B")
    ]

tokenParsersGroup =
  testGroup
    "Tests for parsers that parse Tokens"
    [ testCase "token with input" $
        P.runParser' (L.tkn <$> P.token) [lexInfo $ L.NumberToken 6] @?= Right (L.NumberToken 6, []),
      testCase "token with no more input" $ P.runParser' (L.tkn <$> P.token) [] @?= Left "No more input",
      testCase "numberToken success" $
        P.runParser' (L.tkn <$> P.numberToken) [lexInfo $ L.NumberToken 6] @?= Right (6, []),
      testCase "numberToken failure" $
        P.runParser' (L.tkn <$> P.numberToken) [lexInfo $ L.WordToken "A"] @?= Left "Expected number token",
      testCase "wordToken success" $
        P.runParser' (L.tkn <$> P.wordToken) [lexInfo $ L.WordToken "A"] @?= Right ("A", []),
      testCase "wordToken failure" $
        P.runParser' (L.tkn <$> P.wordToken) [lexInfo $ L.NumberToken 6] @?= Left "Expected word token",
      testCase "operatorToken success" $
        P.runParser' (L.tkn <$> P.operatorToken) [lexInfo $ L.OperatorToken "A"] @?= Right ("A", []),
      testCase "operatorToken failure" $
        P.runParser' (L.tkn <$> P.operatorToken) [lexInfo $ L.NumberToken 6] @?= Left "Expected operator token",
      testCase "bracketToken success" $
        P.runParser' (L.tkn <$> P.bracketToken) [lexInfo $ L.BracketToken "A"] @?= Right ("A", []),
      testCase "bracketToken failure" $
        P.runParser' (L.tkn <$> P.bracketToken) [lexInfo $ L.NumberToken 6] @?= Left "Expected bracket token",
      testCase "end success" $
        P.runParser' P.end [] @?= Right ((), []),
      testCase "end failure" $
        P.runParser' P.end [lexInfo $ L.NumberToken 6] @?= Left "Expected end of input"
    ]

expressionParsersGroup =
  testGroup
    "Tests for parsers that parse (parts of) expressions"
    [ testCase "operator success" $ P.runParser' P.operator [lexInfo $ L.OperatorToken "+"] @?= Right (Plus, []),
      testCase "operator failure" $
        P.runParser' P.operator [lexInfo $ L.OperatorToken "A"] @?= Left "Invalid operator token: 'A'",
      testCase "primitive number" $ P.runParser' P.primitive [lexInfo $ L.NumberToken 6] @?= Right (Number 6, []),
      testCase "primitive number with +" $
        P.runParser' P.primitive (lexInfo <$> [L.OperatorToken "+", L.NumberToken 6]) @?= Right (Number 6, []),
      testCase "primitive negative number" $
        P.runParser' P.primitive (lexInfo <$> [L.OperatorToken "-", L.NumberToken 6]) @?= Right (Number (-6), []),
      testCase "primitive negative number with space" $
        P.runParser' P.primitive [lexInfo $ L.OperatorToken "-", L.LexInfo (L.NumberToken 6) True]
          @?= Left "Expected number token",
      testCase "primitive truth" $ P.runParser' P.primitive [lexInfo $ L.WordToken "YES"] @?= Right (Truth True, []),
      testCase "keyword success" $ P.runParser' P.keyword [lexInfo $ L.WordToken "store"] @?= Right ("store", []),
      testCase "keyword failure" $
        P.runParser' P.keyword [lexInfo $ L.WordToken "notakeyword"] @?= Left "Word 'notakeyword' is not a keyword",
      testCase "bracket success" $ P.runParser' (P.bracket "}") [lexInfo $ L.BracketToken "}"] @?= Right ((), []),
      testCase "bracket wrong bracket" $
        P.runParser' (P.bracket "}") [lexInfo $ L.BracketToken "{"] @?= Left "Expected bracket '}' but got '{'",
      testCase "variable success" $
        P.runParser' P.variable [lexInfo $ L.WordToken "var"] @?= Right (Variable "var", []),
      testCase "variable failure" $
        P.runParser' P.variable [lexInfo $ L.WordToken "load"] @?= Left "Keyword 'load' cannot be a variable",
      -- Expression parsers
      testCase "expression primitive" $
        P.runParser' (P.expression False) [lexInfo $ L.NumberToken 6]
          @?= Right (PrimitiveExpression (Number 6), []),
      testCase "expression variable" $
        P.runParser' (P.expression False) [lexInfo $ L.WordToken "var"]
          @?= Right (VariableReference (Variable "var"), [])
    ]
