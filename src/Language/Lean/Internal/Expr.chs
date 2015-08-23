{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Lean.Internal.Expr
  ( MacroDef
  , Expr
  , ListExpr
    -- * Foreign interface
  , MacroDefPtr
  , OutMacroDefPtr
  , tryAllocMacroDef
  , withMacroDef
  , ExprPtr
  , OutExprPtr
  , tryAllocExpr
  , withExpr
  , ListExprPtr
  , OutListExprPtr
  , tryAllocListExpr
  , withListExpr
  ) where

import Foreign

import Language.Lean.List

{#import Language.Lean.Internal.Exception#}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"
#include "lean_expr.h"

------------------------------------------------------------------------
-- MacroDef declaration

-- | A Lean macro definition
{#pointer lean_macro_def as MacroDef foreign newtype#}
{#pointer lean_macro_def as MacroDefPtr -> MacroDef#}
{#pointer lean_macro_def as OutMacroDefPtr -> MacroDefPtr#}

-- | Call a C layer function that attempts to allocate a
-- new declaration.
tryAllocMacroDef :: LeanPartialFn MacroDefPtr -> MacroDef
tryAllocMacroDef mk =
  MacroDef $ tryAllocLeanValue lean_macro_def_del_ptr mk

foreign import ccall "&lean_macro_def_del"
  lean_macro_def_del_ptr :: FunPtr (MacroDefPtr -> IO ())

------------------------------------------------------------------------
-- Expr declaration

-- | A Lean macro definition
{#pointer lean_expr as Expr foreign newtype#}
{#pointer lean_expr as ExprPtr -> Expr#}
{#pointer lean_expr as OutExprPtr -> ExprPtr#}

-- | Call a C layer function that attempts to allocate a
-- new declaration.
tryAllocExpr :: LeanPartialFn ExprPtr -> Expr
tryAllocExpr mk =
  Expr $ tryAllocLeanValue lean_expr_del_ptr mk

foreign import ccall "&lean_expr_del"
  lean_expr_del_ptr :: FunPtr (ExprPtr -> IO ())

------------------------------------------------------------------------
-- ListExpr declaration

-- | A Lean macro definition
-- | Definition for liss of universes.
newtype instance List Expr = ListExpr (ForeignPtr (List Expr))
-- Synonym for List Expr
type ListExpr = List Expr

withListExpr :: List Expr -> (Ptr (List Expr) -> IO a) -> IO a
withListExpr (ListExpr p) = withForeignPtr p

{#pointer lean_list_expr as ListExpr foreign newtype nocode#}
{#pointer lean_list_expr as ListExprPtr -> ListExpr#}
{#pointer lean_list_expr as OutListExprPtr -> ListExprPtr#}

-- | Call a C layer function that attempts to allocate a
-- new declaration.
tryAllocListExpr :: LeanPartialFn ListExprPtr -> List Expr
tryAllocListExpr mk =
  ListExpr $ tryAllocLeanValue lean_list_expr_del_ptr mk

foreign import ccall "&lean_list_expr_del"
  lean_list_expr_del_ptr :: FunPtr (ListExprPtr -> IO ())
