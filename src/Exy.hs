module Exy (ExyState, Output (..), Primitive(..), Operator, Expression) where

import qualified Data.Text as T

-- Interaction

data Output = Continue | Quit deriving (Eq)

type ExyState = [T.Text]

-- Primitive types

newtype Primitive = Number Integer
data Operator = Plus | Minus

-- Composite types

data Expression = BinaryExpression Operator Primitive Primitive
