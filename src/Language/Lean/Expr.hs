module Language.Lean.Expr
  ( MacroDef
  , Expr
  , BinderKind(..)
    -- * Constructors
  , varExpr
  , sortExpr
  , constExpr
  , appExpr
  , lambdaExpr
  , piExpr
  , macroExpr
  , localExpr
  , localExtExpr
  , metavarExpr
    -- * View
  , ExprView(..)
  , viewExpr
    -- * Operations
  , exprLt
  ) where

import Language.Lean.Internal.Expr
