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
    declType :: Maybe Type,
    declErrors :: [T.Text]
  }

createDeclaration :: ExyState -> Expression -> Declaration
createDeclaration state expr =
  let types = expressionType state expr

      resolvedType =
        case Set.toList types of
          [t] -> Just t
          _ -> Nothing

      typeError =
        case Set.toList types of
          [] -> ["No type could be determined for the expression"]
          [_] -> []
          xs ->
            [ T.pack $
                printf
                  "Expression type could not be uniquely identified, candidates: %s"
                  (unwords $ List.intersperse "," $ map show xs)
            ]
   in Declaration
        { declExpr = expr,
          declType = resolvedType,
          declErrors = typeError
        }

data Type = TypeNumber | TypeTruth deriving (Eq, Ord)

instance Show Type where
  show TypeNumber = "number"
  show TypeTruth = "truth"

expressionType :: ExyState -> Expression -> Set Type
expressionType _ (PrimitiveExpression p) = Set.singleton $ primitiveType p
expressionType state (VariableReference v) = setFromMaybe $ variableType state v
expressionType state (BinaryExpression _ l r) = Set.union (expressionType state l) (expressionType state r)
expressionType state (GroupedExpression e) = expressionType state e

variableType :: ExyState -> Variable -> Maybe Type
variableType state var = declType =<< Map.lookup var state

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

data Operator = Plus | Minus | Equals | Or | And deriving (Show)

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
