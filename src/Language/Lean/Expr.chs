{-|
Module      : Language.Lean.Expr
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Operations for Lean expressions.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wwarn #-}
module Language.Lean.Expr
  ( -- * MacroDef
    MacroDef
  , macroDefToString
    -- * Expressions
  , Expr
  , BinderKind(..)
    -- ** Constructors
  , varExpr
  , sortExpr
  , constExpr
  , appExpr
  , lambdaExpr
  , piExpr
  , macroExpr
  , localConstExpr
  , localExpr
  , localExtExpr
  , metavarExpr
    -- ** View
  , ExprView(..)
  , exprView
    -- ** Operations
  , exprLt
  , exprToString
    -- * Local constant
  , LocalConst
  , localConst
  , localConstExt
  , localConstBinderKind
  , localConstName
  , localConstPrettyName
  , localConstType
  , localConstListToExprList
  ) where

import Foreign
import Foreign.C

import Language.Lean.List
{#import Language.Lean.Internal.Exception#}
import Language.Lean.Internal.Exception.Unsafe
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
-- LocalConst

-- | A local constant.
--
-- Internally, local constants are just expressions, but we use this to
-- refine the types in the interface.
newtype LocalConst = LocalConst Expr

-- | Return the expression representing this local constant.
localConstExpr :: LocalConst -> Expr
localConstExpr (LocalConst e) = e
{-# INLINE localConstExpr #-}

instance Eq LocalConst where
  x == y = localConstExpr x == localConstExpr y

instance Show LocalConst where
  show x = show (localConstExpr x)

instance IsLeanValue LocalConst (Ptr Expr) where
  mkLeanValue p = LocalConst <$> mkLeanValue p

-- | Create a local constant with the given name and type.
localConst :: Name -> Expr -> LocalConst
localConst nm tp = getLeanValue $ lean_expr_mk_local nm tp

{#fun unsafe lean_expr_mk_local
  { `Name'
  , `Expr'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a local constant with additional parameters
localConstExt :: BinderKind -- ^ The binder kind for expression
              -> Name -- ^ The name of expression
              -> Name -- ^ The pretty print name
              -> Expr -- ^ The type of the expression
              -> LocalConst
localConstExt k nm ppnm tp = getLeanValue $ lean_expr_mk_local_ext nm ppnm tp k

{#fun unsafe lean_expr_mk_local_ext
  { `Name'
  , `Name'
  , `Expr'
  , `BinderKind'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Get the kind of binding for the local constant.
localConstBinderKind :: LocalConst -> BinderKind
localConstBinderKind x = getEnum $ lean_expr_get_local_binder_kind $ localConstExpr x

-- | Get the name of the local constant.
localConstName :: LocalConst -> Name
localConstName x = getLeanValue $ lean_expr_get_mlocal_name $ localConstExpr x

-- | Get the name for pretty printing the local constant.
localConstPrettyName :: LocalConst -> Name
localConstPrettyName x = getLeanValue $ lean_expr_get_local_pp_name $ localConstExpr x

-- | Get the type of the local constant.
localConstType :: LocalConst -> Expr
localConstType x = getLeanValue $ lean_expr_get_mlocal_type $ localConstExpr x

------------------------------------------------------------------------
-- List LocalConst

-- | A list of expressions (constructor not actually exported)
newtype instance List LocalConst = ListLocalConst (List Expr)
  deriving ( IsListIso)

-- | Convert a list of local constants to a list of expressions.
localConstListToExprList :: List LocalConst -> List Expr
localConstListToExprList (ListLocalConst l) = l

instance Show (List LocalConst) where
  show (ListLocalConst l) = show l

instance IsList (List LocalConst) where
  type Item (List LocalConst) = LocalConst
  fromList l = ListLocalConst (fromList (localConstExpr <$> l))
  toList (ListLocalConst l) = LocalConst <$> toList l

instance IsLeanValue (List LocalConst) ListExprPtr where
  mkLeanValue p = ListLocalConst <$> mkLeanValue p

------------------------------------------------------------------------
-- Expression constructors

-- | Create a variable with de-Bruijn index @i@. This is a bound variable.
varExpr :: Word32 -> Expr
varExpr i = getLeanValue $ lean_expr_mk_var i

{#fun unsafe lean_expr_mk_var
  { `Word32'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Creates a type for the given universe.
sortExpr :: Univ -> Expr
sortExpr u = getLeanValue $ lean_expr_mk_sort u

{#fun unsafe lean_expr_mk_sort
  { `Univ'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a constant with a given name and universe parameters.
constExpr :: Name -> List Univ -> Expr
constExpr nm params = getLeanValue $ lean_expr_mk_const nm params

{#fun unsafe lean_expr_mk_const
  { `Name'
  , `ListUniv'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a function application for expressions.
appExpr :: Expr -> Expr -> Expr
appExpr f a = getLeanValue $ lean_expr_mk_app f a

{#fun unsafe lean_expr_mk_app
  { `Expr'
  , `Expr'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a lambda abstraction for expressions.
lambdaExpr :: BinderKind -> Name -> Expr -> Expr -> Expr
lambdaExpr k nm tp b = getLeanValue $ lean_expr_mk_lambda nm tp b k

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
piExpr k nm tp b = getLeanValue $ lean_expr_mk_pi nm tp b k

{#fun unsafe lean_expr_mk_pi
  { `Name'
  , `Expr'
  , `Expr'
  , `BinderKind'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a macro application for expressions.
macroExpr :: MacroDef -> List Expr -> Expr
macroExpr m args = getLeanValue $ lean_expr_mk_macro m args

{#fun unsafe lean_expr_mk_macro
  { `MacroDef'
  , `ListExpr'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a metavariable with the given name and type.
metavarExpr :: Name -> Expr -> Expr
metavarExpr nm tp = getLeanValue $ lean_expr_mk_metavar nm tp

{#fun unsafe lean_expr_mk_metavar
  { `Name'
  , `Expr'
  , `OutExprPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a expression from the local cosntant with the given name and type.
--
-- @'localExpr' nm tp@ is equivalent to @'localConstExpr' ('localConst' nm tp)@.
localExpr :: Name -> Expr -> Expr
localExpr nm tp = localConstExpr $ localConst nm tp

-- | Create a local constant with additional parameters.
localExtExpr :: BinderKind -- ^ The binder kind for expression
             -> Name -- ^ The name of expression
             -> Name -- ^ The pretty print name
             -> Expr -- ^ The type of the expression
             -> Expr
localExtExpr k nm ppnm tp = localConstExpr $ localConstExt k nm ppnm tp


------------------------------------------------------------------------
-- Expression view

-- | Information about the expression.
data ExprView
  = ExprVar Word32
  | ExprSort Univ
  | ExprConst Name (List Univ)
  | ExprLocal !LocalConst
  | ExprMeta Name Expr
  | ExprApp Expr Expr
  | ExprLambda BinderKind Name Expr Expr
  | ExprPi     BinderKind Name Expr Expr
  | ExprMacro MacroDef (List Expr)
  deriving (Eq, Show)

-- | View information about the structure of an expression.
exprView :: Expr -> ExprView
exprView x =
  case lean_expr_get_kind x of
    LEAN_EXPR_VAR ->
      ExprVar (getLeanValue $ lean_expr_get_var_idx x)
    LEAN_EXPR_SORT ->
      ExprSort (getLeanValue $ lean_expr_get_sort_univ x)
    LEAN_EXPR_CONST ->
      ExprConst (getLeanValue $ lean_expr_get_const_name x)
                (getLeanValue $ lean_expr_get_const_univs x)
    LEAN_EXPR_LOCAL ->
      ExprLocal (LocalConst x)
    LEAN_EXPR_META ->
      ExprMeta  (getLeanValue $ lean_expr_get_mlocal_name x)
                (getLeanValue $ lean_expr_get_mlocal_type x)
    LEAN_EXPR_APP ->
      ExprApp (getLeanValue $ lean_expr_get_app_fun x)
              (getLeanValue $ lean_expr_get_app_arg x)
    LEAN_EXPR_LAMBDA ->
      ExprLambda (getEnum      $ lean_expr_get_binding_binder_kind x)
                 (getLeanValue $ lean_expr_get_binding_name x)
                 (getLeanValue $ lean_expr_get_binding_domain x)
                 (getLeanValue $ lean_expr_get_binding_body x)
    LEAN_EXPR_PI ->
      ExprPi (getEnum      $ lean_expr_get_binding_binder_kind x)
             (getLeanValue $ lean_expr_get_binding_name x)
             (getLeanValue $ lean_expr_get_binding_domain x)
             (getLeanValue $ lean_expr_get_binding_body x)
--    LEAN_EXPR_LET ->
--      ExprLet undefined
    LEAN_EXPR_MACRO ->
      ExprMacro (getLeanValue $ lean_expr_get_macro_def x)
                (getLeanValue $ lean_expr_get_macro_args x)


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
