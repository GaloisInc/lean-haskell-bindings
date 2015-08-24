{-# LANGUAGE ForeignFunctionInterface #-}
module Language.Lean.Internal.Decl
  ( Env(..)
  , Decl
    -- * Constructors
  , axiomDecl
  , constDecl
  , defDecl
  , defWithDecl
  , thmDecl
  , thmWithDecl
    -- * Projections
  , declName
  , declUnivParams
  , declType
  , DeclView(..)
  , viewDecl
    -- * Certified declarations
  , CertDecl
  , check
    -- * Foreign interface
  , EnvPtr
  , OutEnvPtr
  , withEnv
  , DeclPtr
  , OutDeclPtr
  , allocDecl
  , tryAllocDecl
  , withDecl
  , CertDeclPtr
  , OutCertDeclPtr
  , tryAllocCertDecl
  , withCertDecl
  ) where

import Foreign
import Foreign.C
import System.IO.Unsafe

import Language.Lean.List

{#import Language.Lean.Internal.Exception#}
{#import Language.Lean.Internal.Name#}
{#import Language.Lean.Internal.Expr#}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"
#include "lean_expr.h"
#include "lean_decl.h"

------------------------------------------------------------------------
-- Env declaration

-- | A Lean environment
{#pointer lean_env as Env foreign newtype#}
{#pointer lean_env as EnvPtr -> Env#}
{#pointer *lean_env as OutEnvPtr -> EnvPtr#}

------------------------------------------------------------------------
-- Decl declaration

-- | A Lean declaration
{#pointer lean_decl as Decl foreign newtype#}
{#pointer lean_decl as DeclPtr -> Decl#}
{#pointer *lean_decl as OutDeclPtr -> DeclPtr#}

-- | Create a declaration from a declaration pointer.
allocDecl :: DeclPtr -> IO Decl
allocDecl ptr = Decl <$> newForeignPtr lean_decl_del_ptr ptr

-- | Call a C layer function that attempts to allocate a
-- new declaration.
tryAllocDecl :: LeanPartialFn DeclPtr -> Decl
tryAllocDecl mk =
  Decl $ tryAllocLeanValue lean_decl_del_ptr $ mk

foreign import ccall "&lean_decl_del"
  lean_decl_del_ptr :: FunPtr (DeclPtr -> IO ())


------------------------------------------------------------------------
-- CertDecl declaration

-- | A Lean certified declaration
{#pointer lean_cert_decl as CertDecl foreign newtype#}
{#pointer lean_cert_decl as CertDeclPtr -> CertDecl#}
{#pointer *lean_cert_decl as OutCertDeclPtr -> CertDeclPtr#}

-- | Call a C layer function that attempts to allocate a
-- new certified declaration.
tryAllocCertDecl :: LeanPartialFn CertDeclPtr -> CertDecl
tryAllocCertDecl mk =
  CertDecl $ tryAllocLeanValue lean_cert_decl_del_ptr $ mk

foreign import ccall "&lean_cert_decl_del"
  lean_cert_decl_del_ptr :: FunPtr (CertDeclPtr -> IO ())

------------------------------------------------------------------------
-- Constructors

-- | Create an axiom with name @nm@, universe parameters names
-- @params@, and type @tp@. Note that declartions are universe
-- polymorphic in Lean.
axiomDecl :: Name -> List Name -> Expr -> Decl
axiomDecl nm params tp = tryAllocDecl $ lean_decl_mk_axiom nm params tp

{#fun unsafe lean_decl_mk_axiom
  { `Name'
  , `ListName'
  , `Expr'
  , `OutDeclPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a constant with name @nm@, universe parameters names
-- @params@, and type @tp@. Constants and axioms in Lean are
-- essentially the same thing.
constDecl :: Name -> List Name -> Expr -> Decl
constDecl nm params tp = tryAllocDecl $ lean_decl_mk_const nm params tp

{#fun unsafe lean_decl_mk_const
  { `Name'
  , `ListName'
  , `Expr'
  , `OutDeclPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a definition with name @nm@, universe parameters names
-- @params@, type @tp@, value @v@, definitional height @h@ and flag
-- @o@ indicating whether normalization will lazily unfold it or
-- not.
defDecl :: Name -> List Name -> Expr -> Expr -> Word32 -> Bool -> Decl
defDecl nm params tp v h o = tryAllocDecl $ lean_decl_mk_def nm params tp v h o

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
defWithDecl :: Env -> Name -> List Name -> Expr -> Expr -> Bool -> Decl
defWithDecl e nm params tp v o =
  tryAllocDecl $ lean_decl_mk_def_with e nm params tp v o

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

-- | Create a theorem with name @nm@, universe parameters names
-- @params@, type @tp@, value @v@, definitional height @h@. Theorems
-- and definitions are essentially the same thing in Lean, except in
-- the way the normalizer treats them. The normalizer will only unfold
-- theroem if there is nothing else to be done when checking whether
-- two terms are definitionally equal or not.
thmDecl :: Name -> List Name -> Expr -> Expr -> Word32 -> Decl
thmDecl nm params tp v h = tryAllocDecl $ lean_decl_mk_thm nm params tp v h

{#fun unsafe lean_decl_mk_thm
  { `Name'
  , `ListName'
  , `Expr'
  , `Expr'
  , `Word32'
  , `OutDeclPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a theorem with name @nm@, universe parameters names
-- @params@, type @tp@, value @v@, definitional height @h@. Theorems
-- and definitions are essentially the same thing in Lean, except in
-- the way the normalizer treats them. The normalizer will only unfold
-- theroem if there is nothing else to be done when checking whether
-- two terms are definitionally equal or not. The definitional height
-- is computed from environment.
thmWithDecl :: Env -> Name -> List Name -> Expr -> Expr -> Decl
thmWithDecl e nm params tp v = tryAllocDecl $ lean_decl_mk_thm_with e nm params tp v

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

declName :: Decl -> Name
declName d = tryAllocName $ lean_decl_get_name d

{#fun unsafe lean_decl_get_name
  { `Decl', `OutNamePtr', `OutExceptionPtr' } -> `Bool' #}

declUnivParams :: Decl -> List Name
declUnivParams d = tryAllocListName $ lean_decl_get_univ_params d

{#fun unsafe lean_decl_get_univ_params
  { `Decl', `OutListNamePtr', `OutExceptionPtr' } -> `Bool' #}

declType :: Decl -> Expr
declType d = tryAllocExpr $ lean_decl_get_type d

{#fun unsafe lean_decl_get_type
  { `Decl', `OutExprPtr', `OutExceptionPtr' } -> `Bool' #}

data DeclView
 = DeclConst
 | DeclAxiom
 | DeclDef Expr Word32 Bool
 | DeclThm Expr Word32

viewDecl :: Decl -> DeclView
viewDecl x =
  case lean_decl_get_kind x of
    LEAN_DECL_CONST -> DeclConst
    LEAN_DECL_AXIOM -> DeclAxiom
    LEAN_DECL_DEF ->
      DeclDef (tryAllocExpr $ lean_decl_get_value x)
              (tryGetUInt $ lean_decl_get_height x)
              (tryGetBool $ lean_decl_get_conv_opt x)
    LEAN_DECL_THM ->
      DeclThm (tryAllocExpr $ lean_decl_get_value x)
              (tryGetUInt $ lean_decl_get_height x)

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

check :: Env -> Decl -> CertDecl
check e d = tryAllocCertDecl $ lean_decl_check e d

{#fun unsafe lean_decl_check
  { `Env', `Decl', `OutCertDeclPtr', `OutExceptionPtr' } -> `Bool' #}
