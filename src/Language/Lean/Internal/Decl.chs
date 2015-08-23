{-# LANGUAGE ForeignFunctionInterface #-}
module Language.Lean.Internal.Decl
  ( Env(..)
  , Decl
  , CertDecl
    -- * Foreign interface
  , EnvPtr
  , OutEnvPtr
  , withEnv
  , DeclPtr
  , OutDeclPtr
  , tryAllocDecl
  , withDecl
  , CertDeclPtr
  , OutCertDeclPtr
  , tryAllocCertDecl
  , withCertDecl
  ) where

import Foreign
--import Foreign.C
--import System.IO.Unsafe

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"
#include "lean_expr.h"
#include "lean_decl.h"

{#import Language.Lean.Internal.Exception#}

------------------------------------------------------------------------
-- Env declaration

-- | A Lean environment
{#pointer lean_env as Env foreign newtype#}
{#pointer lean_env as EnvPtr -> Env#}
{#pointer lean_env as OutEnvPtr -> EnvPtr#}

------------------------------------------------------------------------
-- Decl declaration

-- | A Lean declaration
{#pointer lean_decl as Decl foreign newtype#}
{#pointer lean_decl as DeclPtr -> Decl#}
{#pointer lean_decl as OutDeclPtr -> DeclPtr#}

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
{#pointer lean_cert_decl as OutCertDeclPtr -> CertDeclPtr#}

-- | Call a C layer function that attempts to allocate a
-- new certified declaration.
tryAllocCertDecl :: LeanPartialFn CertDeclPtr -> CertDecl
tryAllocCertDecl mk =
  CertDecl $ tryAllocLeanValue lean_cert_decl_del_ptr $ mk

foreign import ccall "&lean_cert_decl_del"
  lean_cert_decl_del_ptr :: FunPtr (CertDeclPtr -> IO ())
