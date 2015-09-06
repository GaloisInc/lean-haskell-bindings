{-|
Module      : Language.Lean.Expr
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Operations for Lean expressions.
-}
{-# LANGUAGE ForeignFunctionInterface #-}
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

import Foreign
import Foreign.C
import System.IO.Unsafe

import Language.Lean.List
{#import Language.Lean.Internal.Exception#}
{#import Language.Lean.Internal.Expr#}
{#import Language.Lean.Internal.Name#}
{#import Language.Lean.Internal.Univ#}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"
#include "lean_expr.h"

------------------------------------------------------------------------
-- Expression constructors

-- | Create a variable with de-Bruijn index @i@. This is a bound variable.
varExpr :: Word32 -> Expr
varExpr i = tryGetLeanValue $ lean_expr_mk_var i

{#fun unsafe lean_expr_mk_var
  { `Word32'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | @sortExpr u@ denotes lean @type u@
sortExpr :: Univ -> Expr
sortExpr u = tryGetLeanValue $ lean_expr_mk_sort u

{#fun unsafe lean_expr_mk_sort
  { `Univ'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a constant with a given name and universe parameters
constExpr :: Name -> List Univ -> Expr
constExpr nm params = tryGetLeanValue $ lean_expr_mk_const nm params

{#fun unsafe lean_expr_mk_const
  { `Name'
  , `ListUniv'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a function application for expressions
appExpr :: Expr -> Expr -> Expr
appExpr f a = tryGetLeanValue $ lean_expr_mk_app f a

{#fun unsafe lean_expr_mk_app
  { `Expr'
  , `Expr'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a lambda abstraction for expressions
lambdaExpr :: BinderKind -> Name -> Expr -> Expr -> Expr
lambdaExpr k nm tp b = tryGetLeanValue $ lean_expr_mk_lambda nm tp b k

{#fun unsafe lean_expr_mk_lambda
  { `Name'
  , `Expr'
  , `Expr'
  , `BinderKind'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a pi abstraction for expressions
piExpr :: BinderKind -> Name -> Expr -> Expr -> Expr
piExpr k nm tp b = tryGetLeanValue $ lean_expr_mk_pi nm tp b k

{#fun unsafe lean_expr_mk_pi
  { `Name'
  , `Expr'
  , `Expr'
  , `BinderKind'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a macro application for expressions
macroExpr :: MacroDef -> List Expr -> Expr
macroExpr m args = tryGetLeanValue $ lean_expr_mk_macro m args

{#fun unsafe lean_expr_mk_macro
  { `MacroDef'
  , `ListExpr'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a local constant with name @nm@ and type @tp@
localExpr :: Name -> Expr -> Expr
localExpr nm tp = tryGetLeanValue $ lean_expr_mk_local nm tp

{#fun unsafe lean_expr_mk_local
  { `Name'
  , `Expr'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | @localExtExpr nm ppnm tp k@ returns a local constant with name
-- @nm@, pretty print name @ppnm@, type @tp@, and binder annotation
-- @k@.
localExtExpr :: BinderKind -> Name -> Name -> Expr -> Expr
localExtExpr k nm ppnm tp = tryGetLeanValue $ lean_expr_mk_local_ext nm ppnm tp k

{#fun unsafe lean_expr_mk_local_ext
  { `Name'
  , `Name'
  , `Expr'
  , `BinderKind'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a metavariable with the given name @nm@ and type @tp@.
metavarExpr :: Name -> Expr -> Expr
metavarExpr nm tp = tryGetLeanValue $ lean_expr_mk_metavar nm tp

{#fun unsafe lean_expr_mk_metavar
  { `Name'
  , `Expr'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

------------------------------------------------------------------------
-- Expression view

-- | Information about the expression.
data ExprView
  = ExprVar Word32
  | ExprSort Univ
  | ExprConst Name (List Univ)
  | ExprLocal BinderKind Name Name Expr
  | ExprMeta Name Expr
  | ExprApp Expr Expr
  | ExprLambda BinderKind Name Expr Expr
  | ExprPi     BinderKind Name Expr Expr
  | ExprMacro MacroDef (List Expr)
  deriving (Eq, Show)

-- | View information about the structure of an expression.
viewExpr :: Expr -> ExprView
viewExpr x =
  case lean_expr_get_kind x of
    LEAN_EXPR_VAR ->
      ExprVar (tryGetLeanValue $ lean_expr_get_var_idx x)
    LEAN_EXPR_SORT ->
      ExprSort (tryGetLeanValue $ lean_expr_get_sort_univ x)
    LEAN_EXPR_CONST ->
      ExprConst (tryGetLeanValue $ lean_expr_get_const_name x)
                (tryGetLeanValue $ lean_expr_get_const_univs x)
    LEAN_EXPR_LOCAL ->
      ExprLocal (tryGetEnum $ lean_expr_get_local_binder_kind x)
                (tryGetLeanValue $ lean_expr_get_mlocal_name x)
                (tryGetLeanValue $ lean_expr_get_local_pp_name x)
                (tryGetLeanValue $ lean_expr_get_mlocal_type x)
    LEAN_EXPR_META ->
      ExprMeta  (tryGetLeanValue $ lean_expr_get_mlocal_name x)
                (tryGetLeanValue $ lean_expr_get_mlocal_type x)
    LEAN_EXPR_APP ->
      ExprApp (tryGetLeanValue $ lean_expr_get_app_fun x)
              (tryGetLeanValue $ lean_expr_get_app_arg x)
    LEAN_EXPR_LAMBDA ->
      ExprLambda (tryGetEnum $ lean_expr_get_binding_binder_kind x)
                 (tryGetLeanValue $ lean_expr_get_binding_name x)
                 (tryGetLeanValue $ lean_expr_get_binding_domain x)
                 (tryGetLeanValue $ lean_expr_get_binding_body x)
    LEAN_EXPR_PI ->
      ExprPi (tryGetEnum $ lean_expr_get_binding_binder_kind x)
             (tryGetLeanValue $ lean_expr_get_binding_name x)
             (tryGetLeanValue $ lean_expr_get_binding_domain x)
             (tryGetLeanValue $ lean_expr_get_binding_body x)
    LEAN_EXPR_MACRO ->
      ExprMacro (tryGetLeanValue $ lean_expr_get_macro_def x)
                (tryGetLeanValue $ lean_expr_get_macro_args x)

{#enum lean_expr_kind as ExprKind { upcaseFirstLetter }
         deriving (Eq)#}

{#fun pure unsafe lean_expr_get_kind
  { `Expr' } -> `ExprKind' #}

{#fun unsafe lean_expr_get_var_idx
  { `Expr', id `Ptr CUInt', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_expr_get_sort_univ
  { `Expr', `OutUnivPtr', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_expr_get_const_name
  { `Expr', `OutNamePtr', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_expr_get_const_univs
  { `Expr', `OutListUnivPtr', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_expr_get_app_fun
  { `Expr', `OutExprPtr', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_expr_get_app_arg
  { `Expr', `OutExprPtr', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_expr_get_mlocal_name
  { `Expr', `OutNamePtr', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_expr_get_mlocal_type
  { `Expr', `OutExprPtr', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_expr_get_local_pp_name
  { `Expr', `OutNamePtr', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_expr_get_local_binder_kind
  { `Expr', id `Ptr CInt', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_expr_get_binding_name
  { `Expr', `OutNamePtr', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_expr_get_binding_domain
  { `Expr', `OutExprPtr', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_expr_get_binding_body
  { `Expr', `OutExprPtr', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_expr_get_binding_binder_kind
  { `Expr', id `Ptr CInt', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_expr_get_macro_def
  { `Expr', `OutMacroDefPtr', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_expr_get_macro_args
  { `Expr', `OutListExprPtr', `OutExceptionPtr' } -> `Bool' #}
