module Exy (ExyState, Output (..), Primitive (..), Operator (..), Variable (..), Expression (..), Statement (..)) where

import Data.Map.Strict (Map)
import qualified Data.Text as T

-- Interaction

data Output = Continue | Quit deriving (Eq)

type ExyState = Map Variable Expression

-- Primitive types

newtype Primitive = Number Integer deriving (Show)

data Operator = Plus | Minus deriving (Show)

newtype Variable = Variable T.Text deriving (Show, Eq, Ord)

-- Composite types

data Expression
  = UnaryExpression Primitive
  | BinaryExpression Operator Primitive Primitive
  deriving (Show)

data Statement
  = Store Variable Expression
  | Load Variable
  deriving (Show)
