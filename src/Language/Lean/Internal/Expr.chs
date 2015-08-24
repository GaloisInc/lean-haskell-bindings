{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Lean.Internal.Expr
  ( MacroDef
  , Expr
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
    -- * Internal interface
  , ListExpr
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

import Control.Lens (toListOf)
import Foreign
import Foreign.C
import GHC.Exts (IsList(..))
import System.IO.Unsafe

import Language.Lean.List

{#import Language.Lean.Internal.Exception#}
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
-- MacroDef declaration

-- | A Lean macro definition
{#pointer lean_macro_def as MacroDef foreign newtype#}
{#pointer lean_macro_def as MacroDefPtr -> MacroDef#}
{#pointer *lean_macro_def as OutMacroDefPtr -> MacroDefPtr#}

-- | Call a C layer function that attempts to allocate a
-- new declaration.
tryAllocMacroDef :: LeanPartialFn MacroDefPtr -> MacroDef
tryAllocMacroDef mk =
  MacroDef $ tryAllocLeanValue lean_macro_def_del_ptr mk

foreign import ccall "&lean_macro_def_del"
  lean_macro_def_del_ptr :: FunPtr (MacroDefPtr -> IO ())

------------------------------------------------------------------------
-- BinderKind declaration
{#enum lean_binder_kind as BinderKind { underscoreToCase, upcaseFirstLetter }
   with prefix = "LEAN_"
   deriving (Eq)#}

------------------------------------------------------------------------
-- Expr declaration

-- | A Lean expression
{#pointer lean_expr as Expr foreign newtype#}
{#pointer lean_expr as ExprPtr -> Expr#}
{#pointer *lean_expr as OutExprPtr -> ExprPtr#}

-- | Call a C layer function that attempts to allocate a
-- new expression
tryAllocExpr :: LeanPartialFn ExprPtr -> Expr
tryAllocExpr mk =
  Expr $ tryAllocLeanValue lean_expr_del_ptr mk

foreign import ccall "&lean_expr_del"
  lean_expr_del_ptr :: FunPtr (ExprPtr -> IO ())

------------------------------------------------------------------------
-- ListExpr declaration

-- | Definition for list of expressions
newtype instance List Expr = ListExpr (ForeignPtr (List Expr))
-- Synonym for List Expr
type ListExpr = List Expr

withListExpr :: List Expr -> (Ptr (List Expr) -> IO a) -> IO a
withListExpr (ListExpr p) = withForeignPtr p

{#pointer lean_list_expr as ListExpr foreign newtype nocode#}
{#pointer lean_list_expr as ListExprPtr -> ListExpr#}
{#pointer *lean_list_expr as OutListExprPtr -> ListExprPtr#}

-- | Call a C layer function that attempts to allocate a
-- list of expressions
tryAllocListExpr :: LeanPartialFn ListExprPtr -> List Expr
tryAllocListExpr mk =
  ListExpr $ tryAllocLeanValue lean_list_expr_del_ptr mk

foreign import ccall "&lean_list_expr_del"
  lean_list_expr_del_ptr :: FunPtr (ListExprPtr -> IO ())

------------------------------------------------------------------------
-- Expression constructors

-- | Create a variable with de-Bruijn index @i@. This is a bound variable.
varExpr :: Word32 -> Expr
varExpr i = tryAllocExpr $ lean_expr_mk_var i

{#fun unsafe lean_expr_mk_var
  { `Word32'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | @sortExpr u@ denotes lean @type u@
sortExpr :: Univ -> Expr
sortExpr u = tryAllocExpr $ lean_expr_mk_sort u

{#fun unsafe lean_expr_mk_sort
  { `Univ'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a constant with a given name and universe parameters
constExpr :: Name -> List Univ -> Expr
constExpr nm params = tryAllocExpr $ lean_expr_mk_const nm params

{#fun unsafe lean_expr_mk_const
  { `Name'
  , `ListUniv'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a function application for expressions
appExpr :: Expr -> Expr -> Expr
appExpr f a = tryAllocExpr $ lean_expr_mk_app f a

{#fun unsafe lean_expr_mk_app
  { `Expr'
  , `Expr'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a lambda abstraction for expressions
lambdaExpr :: Name -> Expr -> Expr -> BinderKind -> Expr
lambdaExpr nm tp b k = tryAllocExpr $ lean_expr_mk_lambda nm tp b k

{#fun unsafe lean_expr_mk_lambda
  { `Name'
  , `Expr'
  , `Expr'
  , `BinderKind'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a pi abstraction for expressions
piExpr :: Name -> Expr -> Expr -> BinderKind -> Expr
piExpr nm tp b k = tryAllocExpr $ lean_expr_mk_pi nm tp b k

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
macroExpr m args = tryAllocExpr $ lean_expr_mk_macro m args

{#fun unsafe lean_expr_mk_macro
  { `MacroDef'
  , `ListExpr'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a local constant with name @nm@ and type @tp@
localExpr :: Name -> Expr -> Expr
localExpr nm tp = tryAllocExpr $ lean_expr_mk_local nm tp

{#fun unsafe lean_expr_mk_local
  { `Name'
  , `Expr'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | @localExtExpr nm ppnm tp k@ returns a local constant with name
-- @nm@, pretty print name @ppnm@, type @tp@, and binder annotation
-- @k@.
localExtExpr :: Name -> Name -> Expr -> BinderKind -> Expr
localExtExpr nm ppnm tp k = tryAllocExpr $ lean_expr_mk_local_ext nm ppnm tp k

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
metavarExpr nm tp = tryAllocExpr $ lean_expr_mk_metavar nm tp

{#fun unsafe lean_expr_mk_metavar
  { `Name'
  , `Expr'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

------------------------------------------------------------------------
-- Expression Show instance

instance Show Expr where
  show x = tryAllocString $ lean_expr_to_string x

{#fun unsafe lean_expr_to_string
  { `Expr'
  , id `Ptr CString'
  , `OutExceptionPtr'
  } -> `Bool' #}

------------------------------------------------------------------------
-- Expression Comparison

instance Eq Expr where
  (==) = lean_expr_eq

{#fun pure unsafe lean_expr_eq { `Expr' , `Expr' } -> `Bool' #}


instance Ord Expr where
   x <= y = not (lean_expr_quick_lt y x)

{#fun pure unsafe lean_expr_quick_lt { `Expr' , `Expr' } -> `Bool' #}

------------------------------------------------------------------------
-- Expression view

data ExprView
  = ExprVar Word32
  | ExprSort Univ
  | ExprConst Name (List Univ)
  | ExprLocal Name Name Expr BinderKind
  | ExprMeta Name Expr
  | ExprApp Expr Expr
  | ExprLambda Name Expr Expr BinderKind
  | ExprPi Name Expr Expr BinderKind
  | ExprMacro MacroDef (List Expr)

viewExpr :: Expr -> ExprView
viewExpr x =
  case lean_expr_get_kind x of
    LEAN_EXPR_VAR ->
      ExprVar (tryGetUInt $ lean_expr_get_var_idx x)
    LEAN_EXPR_SORT ->
      ExprSort (tryAllocUniv $ lean_expr_get_sort_univ x)
    LEAN_EXPR_CONST ->
      ExprConst (tryAllocName $ lean_expr_get_const_name x)
                (tryAllocListUniv $ lean_expr_get_const_univs x)
    LEAN_EXPR_LOCAL ->
      ExprLocal (tryAllocName $ lean_expr_get_mlocal_name x)
                (tryAllocName $ lean_expr_get_local_pp_name x)
                (tryAllocExpr $ lean_expr_get_mlocal_type x)
                (tryGetEnum   $ lean_expr_get_local_binder_kind x)
    LEAN_EXPR_META ->
      ExprMeta  (tryAllocName $ lean_expr_get_mlocal_name x)
                (tryAllocExpr $ lean_expr_get_mlocal_type x)
    LEAN_EXPR_APP ->
      ExprApp (tryAllocExpr $ lean_expr_get_app_fun x)
              (tryAllocExpr $ lean_expr_get_app_arg x)
    LEAN_EXPR_LAMBDA ->
      ExprLambda (tryAllocName $ lean_expr_get_binding_name x)
                 (tryAllocExpr $ lean_expr_get_binding_domain x)
                 (tryAllocExpr $ lean_expr_get_binding_body x)
                 (tryGetEnum   $ lean_expr_get_binding_binder_kind x)
    LEAN_EXPR_PI ->
      ExprPi (tryAllocName $ lean_expr_get_binding_name x)
             (tryAllocExpr $ lean_expr_get_binding_domain x)
             (tryAllocExpr $ lean_expr_get_binding_body x)
             (tryGetEnum   $ lean_expr_get_binding_binder_kind x)
    LEAN_EXPR_MACRO ->
      ExprMacro (tryAllocMacroDef $ lean_expr_get_macro_def x)
                (tryAllocListExpr $ lean_expr_get_macro_args x)

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

------------------------------------------------------------------------
-- ListExpr Eq instance

instance Eq (List Expr) where
  (==) = lean_list_expr_eq

{#fun pure unsafe lean_list_expr_eq
   { `ListExpr'
   , `ListExpr'
   } -> `Bool' #}

------------------------------------------------------------------------
-- ListExpr IsListIso instance

instance IsListIso (List Expr) Expr where
  nil = tryAllocListExpr $ lean_list_expr_mk_nil
  h <| r = tryAllocListExpr $ lean_list_expr_mk_cons h r

  viewList l =
    if lean_list_expr_is_cons l then
      tryAllocExpr (lean_list_expr_head l)
        :< tryAllocListExpr (lean_list_expr_tail l)
    else
      Nil

{#fun unsafe lean_list_expr_mk_nil
   { `OutListExprPtr'
   , `OutExceptionPtr'
   } -> `Bool' #}

{#fun unsafe lean_list_expr_mk_cons
   { `Expr'
   , `ListExpr'
   , `OutListExprPtr'
   , `OutExceptionPtr'
   } -> `Bool' #}

{#fun pure unsafe lean_list_expr_is_cons
   { `ListExpr'
   } -> `Bool' #}

{#fun unsafe lean_list_expr_head
   { `ListExpr'
   , `OutExprPtr'
   , `OutExceptionPtr'
   } -> `Bool' #}

{#fun unsafe lean_list_expr_tail
   { `ListExpr'
   , `OutListExprPtr'
   , `OutExceptionPtr'
   } -> `Bool' #}

------------------------------------------------------------------------
-- ListExpr IsList instance

instance IsList (List Expr) where
  type Item ListExpr = Expr
  fromList = fromListDefault
  toList = toListOf traverseList

------------------------------------------------------------------------
-- ListExpr Show instance

instance Show (List Expr) where
  showsPrec _ l = showList (toList l)


