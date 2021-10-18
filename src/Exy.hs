module Exy
  ( ShowExpr,
    showExpr,
    ExyState,
    Output (..),
    Primitive (..),
    Operator (..),
    Variable (..),
    Expression (..),
    Statement (..),
  )
where

import Data.Map.Strict (Map)
import qualified Data.Text as T
import Text.Printf (printf)

class ShowExpr a where
  showExpr :: a -> T.Text

-- Interaction

data Output = Continue | Quit deriving (Eq)

type ExyState = Map Variable Expression

-- Primitive types

newtype Primitive = Number Integer deriving (Show)

instance ShowExpr Primitive where
  showExpr (Number n) = T.pack $ show n

data Operator = Plus | Minus deriving (Show)

instance ShowExpr Operator where
  showExpr Plus = "+"
  showExpr Minus = "-"

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
