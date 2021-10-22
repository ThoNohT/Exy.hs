module Exy
  ( ShowExpr,
    showExpr,
    clearDeclaration,
    insertDependencies,
    dependencies,
    allDependencies,
    recalculateState,
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
import Data.Maybe (mapMaybe)
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import qualified Data.Text as T
import Text.Printf (printf)

class ShowExpr a where
  showExpr :: a -> T.Text

-- Interaction

data Output = Continue | Quit deriving (Eq)

type ExyState = Map Variable Declaration

-- | Specifies that variable `var` is dependent on all variables in `deps`.
-- The state is updated to add `var` as a dependency to all variables already in the state. If a variable
-- is not yet in the state, it is added as an `UndeclaredDeclaration` with this variable as dependency.
insertDependencies :: Variable -> Set Variable -> ExyState -> ExyState
insertDependencies var deps state = Set.fold injectDep state deps
  where
    injectDep :: Variable -> ExyState -> ExyState
    injectDep dep state =
      case Map.lookup dep state of
        Nothing -> Map.insert dep (UndeclaredDeclaration {declDependents = Set.singleton var}) state
        Just decl -> Map.insert var (registerDependency var decl) state

-- | Recalculates the state, given that the provided variables were just inserted or updated.
-- All declarations that depend on this variable are updated, and they in turn trigger the recalculation.
recalculateState :: Set Variable -> ExyState -> ExyState
recalculateState vars state =
  let toUpdate = Set.unions $ declDependents <$> mapMaybe (`Map.lookup` state) (Set.toList vars)
      recalculateDecl st var =
        case Map.lookup var st of
          Nothing -> st
          Just decl ->
            case tryDeclExpr decl of
              Just expr ->
                Map.insert var (decl {declType = expressionType st expr}) st
              Nothing ->
                st
   in -- TODO: This only does one step, but we need it to recurse until there is nothing more to update.
      foldl recalculateDecl state toUpdate

-- TODO: Store computed values and update them when triggered by a dependency.
data Declaration
  = UndeclaredDeclaration {declDependents :: Set Variable}
  | DeclaredDeclaration
      { declExpr :: Expression,
        declType :: Either T.Text Type,
        declDependents :: Set Variable
      }

-- | Attempts to get the expression of a declaration. Returns None if no expression is declared.
tryDeclExpr :: Declaration -> Maybe Expression
tryDeclExpr = \case
  DeclaredDeclaration {declExpr = e} -> Just e
  _ -> Nothing

-- | Attempts to get the type of a declaration. Returns None if no expression is declared.
tryDecltype :: Declaration -> Maybe Type
tryDecltype = \case
  DeclaredDeclaration {declType = t} -> rightToMaybe t
  _ -> Nothing

-- | Registers a variable as a dependency with a declaration.
registerDependency :: Variable -> Declaration -> Declaration
registerDependency dependent = \case
  d@UndeclaredDeclaration {declDependents = dd} -> d {declDependents = Set.insert dependent dd}
  d@DeclaredDeclaration {declDependents = dd} -> d {declDependents = Set.insert dependent dd}

-- | Creates a new declaration for a variable and expression given a state.
-- If the declaration already exists in the sate, its depenencies are copied.
createDeclaration :: Variable -> ExyState -> Expression -> Declaration
createDeclaration var state expr =
  DeclaredDeclaration
    { declExpr = expr,
      declType = expressionType state expr,
      declDependents = maybe Set.empty declDependents (Map.lookup var state)
    }

-- | Clears a declaration for a varable.
clearDeclaration :: Variable -> ExyState -> ExyState
clearDeclaration var state =
  case Map.lookup var state of
    Just DeclaredDeclaration {declDependents = dd} | Set.null dd -> Map.delete var state
    Just DeclaredDeclaration {declDependents = dd} ->
      Map.insert var (UndeclaredDeclaration {declDependents = dd}) state
    _ -> state

-- Type checking

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
variableType state var = tryDecltype =<< Map.lookup var state

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

-- | Returns all variables on which the specified expression directly depends.
dependencies :: Expression -> Set Variable
dependencies (PrimitiveExpression _) = Set.empty
dependencies (VariableReference v) = Set.singleton v
dependencies (BinaryExpression _ l r) = Set.union (dependencies l) (dependencies r)
dependencies (GroupedExpression e) = dependencies e

-- | Returns all dependencies on which the specified expression depends either direcly or indirectly.
-- Circular dependencies are only reported once (but make sure they don't exist anyway).
allDependencies :: ExyState -> Expression -> Set Variable
allDependencies state expr =
  let -- Returns an empty set of dependencies if the variable has no declared expression.
      dependenciesForVariable var = maybe Set.empty dependencies (tryDeclExpr =<< Map.lookup var state)
      directDependencies = dependencies expr

      step :: Set Variable -> Set Variable -> Set Variable
      step all new =
        if Set.null new
          then -- No new dependencies, so we are done expanding.
            all
          else -- There are new dependencies, add them.
          -- But only mark those as new that we haven't seen before.

            let newDeps = Set.unions $ Set.map dependenciesForVariable new
             in step (Set.union all newDeps) (newDeps \\ all)
   in step directDependencies directDependencies

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
