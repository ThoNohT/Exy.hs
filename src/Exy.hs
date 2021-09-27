module Exy (ExyState, Output (..)) where

import qualified Data.Text as T

data Output = Continue | Quit deriving (Eq)

type ExyState = [T.Text]
