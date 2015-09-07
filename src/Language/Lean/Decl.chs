{-|
Module      : Language.Lean.Decl
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Operations for working with Lean declarations.
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Language.Lean.Decl
  ( Env(..)
  , Decl
    -- * Constructors
  , axiom
  , constant
  , definition
  , definitionWith
  , theorem
  , theoremWith
    -- * Projections
  , declName
  , declUnivParams
  , declType
  , DeclView(..)
  , declView
    -- * Certified declarations
  , CertDecl
  , check
  ) where

import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import Language.Lean.List

{#import Language.Lean.Internal.Decl#}
{#import Language.Lean.Internal.Exception#}
{#import Language.Lean.Internal.Expr#}
{#import Language.Lean.Internal.Name#}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"
#include "lean_expr.h"
#include "lean_decl.h"

------------------------------------------------------------------------
-- Constructors

-- | Create an axiom with the given name, universe parameters, and type.
axiom :: Name -> List Name -> Expr -> Decl
axiom nm params tp = tryGetLeanValue $ lean_decl_mk_axiom nm params tp

{#fun unsafe lean_decl_mk_axiom
  { `Name'
  , `ListName'
  , `Expr'
  , `OutDeclPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a constant with the given name, universe parameters,
-- and type @tp@.
--
-- Constants and axioms in Lean are essentially the same thing.
constant :: Name -> List Name -> Expr -> Decl
constant nm params tp = tryGetLeanValue $ lean_decl_mk_const nm params tp

{#fun unsafe lean_decl_mk_const
  { `Name'
  , `ListName'
  , `Expr'
  , `OutDeclPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | @definition nm params tp v h o@ create a definition with name @nm@,
-- universe parameters @params@, type @tp@, value @v@, definitional height @h@
-- and flag @o@ indicating whether normalization will lazily unfold it or not.
definition :: Name -> List Name -> Expr -> Expr -> Word32 -> Bool -> Decl
definition nm params tp v h o = tryGetLeanValue $ lean_decl_mk_def nm params tp v h o

{#fun unsafe lean_decl_mk_def
  { `Name'
  , `ListName'
  , `Expr'
  , `Expr'
  , `Word32'
  , `Bool'
  , `OutDeclPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a definition with name @nm@, universe parameters names
-- @params@, type @tp@, value @v@, and flag @o@ indicating whether
-- normalization will lazily unfold it or not. The definitional height
-- is computed using information from the environment.
definitionWith :: Env -> Name -> List Name -> Expr -> Expr -> Bool -> Decl
definitionWith e nm params tp v o =
  tryGetLeanValue $ lean_decl_mk_def_with e nm params tp v o

{#fun unsafe lean_decl_mk_def_with
  { `Env'
  , `Name'
  , `ListName'
  , `Expr'
  , `Expr'
  , `Bool'
  , `OutDeclPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | @theorem nm params tp v h@ creates a theorem with name @nm@, universe parameters
-- @params@, type @tp@, value @v@, and definitional height @h@.
--
-- Theorems and definitions are essentially the same thing in Lean, except in
-- the way the normalizer treats them. The normalizer will only unfold theroem
-- if there is nothing else to be done when checking whether two terms are
-- definitionally equal or not.
theorem :: Name -> List Name -> Expr -> Expr -> Word32 -> Decl
theorem nm params tp v h = tryGetLeanValue $ lean_decl_mk_thm nm params tp v h

{#fun unsafe lean_decl_mk_thm
  { `Name'
  , `ListName'
  , `Expr'
  , `Expr'
  , `Word32'
  , `OutDeclPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | @theoremWith e nm params tp v@ creates a theorem relative to environment @e@
-- with name @nm@, universe @params@, type @tp@, and value @v@.
--
-- Theorems and definitions are essentially the same thing in Lean, except in
-- the way the normalizer treats them. The normalizer will only unfold theroem
-- if there is nothing else to be done when checking whether two terms are
-- definitionally equal or not.
--
-- The definitional height is computed from environment.
theoremWith :: Env -> Name -> List Name -> Expr -> Expr -> Decl
theoremWith e nm params tp v = tryGetLeanValue $ lean_decl_mk_thm_with e nm params tp v

{#fun unsafe lean_decl_mk_thm_with
  { `Env'
  , `Name'
  , `ListName'
  , `Expr'
  , `Expr'
  , `OutDeclPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

------------------------------------------------------------------------
-- Projections

-- | The name of a declaration.
declName :: Decl -> Name
declName d = tryGetLeanValue $ lean_decl_get_name d

{#fun unsafe lean_decl_get_name
  { `Decl', `OutNamePtr', `OutExceptionPtr' } -> `Bool' #}

-- | The list of universe params for a declaration.
declUnivParams :: Decl -> List Name
declUnivParams d = tryGetLeanValue $ lean_decl_get_univ_params d

{#fun unsafe lean_decl_get_univ_params
  { `Decl', `OutListNamePtr', `OutExceptionPtr' } -> `Bool' #}

-- | The type of a declaration.
declType :: Decl -> Expr
declType d = tryGetLeanValue $ lean_decl_get_type d

{#fun unsafe lean_decl_get_type
  { `Decl', `OutExprPtr', `OutExceptionPtr' } -> `Bool' #}

-- | Information about a declaration
data DeclView
    -- | A constant
  = Constant
    -- | An axiom
  | Axiom
    -- | A definition with the associated value, definitional height, and
    -- whether to lazy unfold it.
  | Definition Expr Word32 Bool
    -- | A theorem
  | Theorem Expr Word32
 deriving (Eq, Show)

-- | Return information about a declaration.
declView :: Decl -> DeclView
declView x =
  case lean_decl_get_kind x of
    LEAN_DECL_CONST -> Constant
    LEAN_DECL_AXIOM -> Axiom
    LEAN_DECL_DEF ->
      Definition (tryGetLeanValue $ lean_decl_get_value x)
                 (tryGetLeanValue $ lean_decl_get_height x)
                 (tryGetLeanValue $ lean_decl_get_conv_opt x)
    LEAN_DECL_THM ->
      Theorem (tryGetLeanValue $ lean_decl_get_value x)
              (tryGetLeanValue $ lean_decl_get_height x)

{#enum lean_decl_kind as DeclKind { upcaseFirstLetter }
         deriving (Eq)#}

{#fun pure unsafe lean_decl_get_kind
  { `Decl' } -> `DeclKind' #}

{#fun unsafe lean_decl_get_value
  { `Decl', `OutExprPtr', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_decl_get_height
  { `Decl', id `Ptr CUInt', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_decl_get_conv_opt
  { `Decl', id `Ptr CInt', `OutExceptionPtr' } -> `Bool' #}

------------------------------------------------------------------------
-- Certified declarations

-- | Creates a cerified declaration by type checking it within a
-- given environment.
check :: Env -> Decl -> CertDecl
check e d = tryGetLeanValue $ lean_decl_check e d

{#fun unsafe lean_decl_check
  { `Env', `Decl', `OutCertDeclPtr', `OutExceptionPtr' } -> `Bool' #}
