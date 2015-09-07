{-|
Module      : Language.Lean.Expr
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Internal declarations for Lean expressions and typeclass instances for @Expr@.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Lean.Internal.Expr
  ( -- * Macro definitions
    MacroDef
  , MacroDefPtr
  , OutMacroDefPtr
  , withMacroDef
    -- * Expressions
  , Expr
  , ExprPtr
  , OutExprPtr
  , withExpr
  , exprLt
  , exprToString
  , BinderKind(..)
    -- * List of expressions
  , ListExpr
  , ListExprPtr
  , OutListExprPtr
  , withListExpr
  ) where

import Control.Lens (toListOf)
import Foreign
import Foreign.C
import GHC.Exts (IsList(..))
import System.IO.Unsafe

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

{#pointer lean_macro_def as MacroDef foreign newtype nocode#}

-- | A Lean macro definition
newtype MacroDef = MacroDef (ForeignPtr MacroDef)

-- | Function @c2hs@ uses to pass @MacroDef@ values to Lean
withMacroDef :: MacroDef -> (Ptr MacroDef -> IO a) -> IO a
withMacroDef (MacroDef o) = withForeignPtr o

-- | Haskell type for @lean_macro_def@ FFI parameters.
{#pointer lean_macro_def as MacroDefPtr -> MacroDef#}
-- | Haskell type for @lean_macro_def*@ FFI parameters.
{#pointer *lean_macro_def as OutMacroDefPtr -> MacroDefPtr#}

instance IsLeanValue MacroDef (Ptr MacroDef) where
  mkLeanValue = fmap MacroDef . newForeignPtr lean_macro_def_del_ptr

foreign import ccall unsafe "&lean_macro_def_del"
  lean_macro_def_del_ptr :: FunPtr (MacroDefPtr -> IO ())

------------------------------------------------------------------------
-- MacroDef eq

instance Eq MacroDef where
  (==) = error "Equality comparison with macro definitions is not yet implemented."

instance Show MacroDef where
  show = error "MacroDef.show not yet implement"

------------------------------------------------------------------------
-- BinderKind declaration

-- | Kind of binding used for bound variables.
{#enum lean_binder_kind as BinderKind { underscoreToCase, upcaseFirstLetter }
   with prefix = "LEAN_"
   deriving (Eq, Show)#}

------------------------------------------------------------------------
-- Expr declaration

{#pointer lean_expr as Expr foreign newtype nocode#}

-- | A Lean expression
newtype Expr = Expr (ForeignPtr Expr)

-- | Get access to @lean_expr@ within IO action.
withExpr :: Expr -> (Ptr Expr -> IO a) -> IO a
withExpr (Expr o) = withForeignPtr o

-- | Haskell type for @lean_expr@ FFI parameters.
{#pointer lean_expr as ExprPtr -> Expr#}
-- | Haskell type for @lean_expr*@ FFI parameters.
{#pointer *lean_expr as OutExprPtr -> ExprPtr#}

instance IsLeanValue Expr (Ptr Expr) where
  mkLeanValue = fmap Expr . newForeignPtr lean_expr_del_ptr

foreign import ccall unsafe "&lean_expr_del"
  lean_expr_del_ptr :: FunPtr (ExprPtr -> IO ())

------------------------------------------------------------------------
-- ListExpr declaration

-- | A list of expressions (constructor not actually exported)
newtype instance List Expr = ListExpr (ForeignPtr (List Expr))

-- | Synonym for @List Expr@ that can be used in @c2hs@ bindings
type ListExpr = List Expr

-- | Function @c2hs@ uses to pass @ListExpr@ values to Lean
withListExpr :: List Expr -> (Ptr (List Expr) -> IO a) -> IO a
withListExpr (ListExpr p) = withForeignPtr p

{#pointer lean_list_expr as ListExpr foreign newtype nocode#}

-- | Haskell type for @lean_list_expr@ FFI parameters.
{#pointer lean_list_expr as ListExprPtr -> ListExpr#}
-- | Haskell type for @lean_list_expr*@ FFI parameters.
{#pointer *lean_list_expr as OutListExprPtr -> ListExprPtr#}

instance IsLeanValue (List Expr) (Ptr (List Expr)) where
  mkLeanValue = fmap ListExpr . newForeignPtr lean_list_expr_del_ptr

foreign import ccall unsafe "&lean_list_expr_del"
  lean_list_expr_del_ptr :: FunPtr (ListExprPtr -> IO ())

------------------------------------------------------------------------
-- Expression Show instance

exprToString :: Expr -> String
exprToString x = tryGetLeanValue $ lean_expr_to_string x

instance Show Expr where
  show = show . exprToString

{#fun unsafe lean_expr_to_string
 { `Expr' , id `Ptr CString', `OutExceptionPtr' } -> `Bool' #}

------------------------------------------------------------------------
-- Expression Comparison

instance Eq Expr where
  x == y = tryGetLeanValue $ lean_expr_eq x y

{#fun unsafe lean_expr_eq
 { `Expr' , `Expr', id `Ptr CInt', `OutExceptionPtr' } -> `Bool' #}

instance Ord Expr where
   x <= y = not $ tryGetLeanValue $ lean_expr_quick_lt y x

{#fun unsafe lean_expr_quick_lt
 { `Expr' , `Expr', id `Ptr CInt', `OutExceptionPtr' } -> `Bool' #}

-- | Return true if first expression is structurally less than other.
exprLt :: Expr -> Expr -> Bool
exprLt x y = tryGetLeanValue $ lean_expr_lt x y

{#fun unsafe lean_expr_lt
 { `Expr' , `Expr', id `Ptr CInt', `OutExceptionPtr' } -> `Bool' #}

------------------------------------------------------------------------
-- ListExpr Eq instance

instance Eq (List Expr) where
  (==) = lean_list_expr_eq

{#fun pure unsafe lean_list_expr_eq
 { `ListExpr', `ListExpr' } -> `Bool' #}

------------------------------------------------------------------------
-- ListExpr IsListIso instance

instance IsListIso (List Expr) Expr where
  nil = tryGetLeanValue $ lean_list_expr_mk_nil
  h <| r = tryGetLeanValue $ lean_list_expr_mk_cons h r

  listView l =
    if lean_list_expr_is_cons l then
      tryGetLeanValue (lean_list_expr_head l)
        :< tryGetLeanValue (lean_list_expr_tail l)
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
