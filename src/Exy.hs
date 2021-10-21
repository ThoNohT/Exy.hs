module Exy
  ( ShowExpr,
    showExpr,
    ExyState,
    Output (..),
    Primitive (..),
    Operator (..),
    Variable (..),
    Expression (..),
    Declaration (..),
    Statement (..),
    expressionType,
    createDeclaration,
  )
where

import Core (setFromMaybe)
import Data.Either.Combinators (maybeToRight, rightToMaybe)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Text.Printf (printf)

class ShowExpr a where
  showExpr :: a -> T.Text

-- Interaction

data Output = Continue | Quit deriving (Eq)

type ExyState = Map Variable Declaration

data DeclarationError
  = ParseError T.Text
  | TypeError T.Text

-- TODO: Store dependent variables so we can store computed values and update all dependencies.
data Declaration = Declaration
  { declExpr :: Expression,
    declType :: Either T.Text Type
  }

createDeclaration :: ExyState -> Expression -> Declaration
createDeclaration state expr =
  Declaration
    { declExpr = expr,
      declType = expressionType state expr
    }

data Type = TypeNumber | TypeTruth deriving (Eq, Ord)

instance Show Type where
  show TypeNumber = "number"
  show TypeTruth = "truth"

expressionType :: ExyState -> Expression -> Either T.Text Type
expressionType _ (PrimitiveExpression p) = Right $ primitiveType p
expressionType state (VariableReference v) =
  maybeToRight (T.pack $ printf "Could not determine type of variable '%s'" (showExpr v)) $ variableType state v
expressionType state (GroupedExpression e) = expressionType state e
expressionType state ex@(BinaryExpression op l r) =
  case (expressionType state l, expressionType state r) of
    (Right tl, Right tr) ->
      maybeToRight (T.pack $ printf "Operator %s does not support types %s and %s in expression %s" (show op) (show tl) (show tr) (showExpr ex)) $
        Map.lookup (op, tl, tr) operatorTypeMap
    (Left el, Left er) -> Left $ T.pack $ unlines [T.unpack el, T.unpack er]
    (Left el, _) -> Left el
    (_, Left er) -> Left er

operatorTypeMap :: Map (Operator, Type, Type) Type
operatorTypeMap =
  Map.fromList
    [ ((Plus, TypeNumber, TypeNumber), TypeNumber),
      ((Minus, TypeNumber, TypeNumber), TypeNumber),
      ((Equals, TypeNumber, TypeNumber), TypeTruth),
      ((Equals, TypeTruth, TypeTruth), TypeTruth),
      ((Or, TypeTruth, TypeTruth), TypeTruth),
      ((And, TypeTruth, TypeTruth), TypeTruth)
    ]

variableType :: ExyState -> Variable -> Maybe Type
variableType state var = ( rightToMaybe . declType ) =<< Map.lookup var state

primitiveType :: Primitive -> Type
primitiveType (Number _) = TypeNumber
primitiveType (Truth _) = TypeTruth

-- Primitive types

data Primitive
  = Number Integer
  | Truth Bool
  deriving (Show)

instance ShowExpr Primitive where
  showExpr (Number n) = T.pack $ show n
  showExpr (Truth True) = "Yes"
  showExpr (Truth False) = "No"

data Operator = Plus | Minus | Equals | Or | And deriving (Show, Eq, Ord)

instance ShowExpr Operator where
  showExpr Plus = "+"
  showExpr Minus = "-"
  showExpr Equals = "="
  showExpr Or = "|"
  showExpr And = "&"

newtype Variable = Variable T.Text deriving (Show, Eq, Ord)

instance ShowExpr Variable where
  showExpr (Variable v) = v

-- Composite types

data Expression
  = PrimitiveExpression Primitive
  | VariableReference Variable
  | BinaryExpression Operator Expression Expression
  | GroupedExpression Expression
  deriving (Show)

instance ShowExpr Expression where
  showExpr (PrimitiveExpression p) = showExpr p
  showExpr (VariableReference v) = showExpr v
  showExpr (BinaryExpression o el er) = T.pack $ printf "%s %s %s" (showExpr el) (showExpr o) (showExpr er)
  showExpr (GroupedExpression g) = T.pack $ printf "( %s )" (showExpr g)

data Statement
  = Store Variable Expression
  | Load Variable
  | Clear Variable
  deriving (Show)
