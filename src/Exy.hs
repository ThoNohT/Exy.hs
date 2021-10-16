module Exy (ExyState, Output (..), Primitive(..), Operator(..), Expression(..)) where

import qualified Data.Text as T

-- Interaction

data Output = Continue | Quit deriving (Eq)

type ExyState = [Expression]

-- Primitive types

newtype Primitive = Number Integer deriving (Show)
data Operator = Plus | Minus deriving (Show)
newtype Variable = Variable T.Text

-- Composite types

data Expression
  = UnaryExpression Primitive
  | BinaryExpression Operator Primitive Primitive deriving (Show)


data Statement
  = Store Variable Expression
  | Load Variable
