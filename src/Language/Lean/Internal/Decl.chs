{-|
Module      : Language.Lean.Internal.Decl
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Declares internal datatypes for Lean environment, declarations, and
certified declarations.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}
module Language.Lean.Internal.Decl
    -- * Declaration
  ( Decl
  , DeclPtr
  , OutDeclPtr
  , withDecl
    -- * Certified declaration
  , CertDecl
  , CertDeclPtr
  , OutCertDeclPtr
  , withCertDecl
  ) where

import Foreign

{#import Language.Lean.Internal.Exception#}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"
#include "lean_expr.h"
#include "lean_decl.h"

------------------------------------------------------------------------
-- Decl declaration

{#pointer lean_decl as Decl foreign newtype nocode#}

-- | A Lean declaration
newtype Decl = Decl (ForeignPtr Decl)

-- | Function @c2hs@ uses to pass @Decl@ values to Lean
withDecl :: Decl -> (Ptr Decl -> IO a) -> IO a
withDecl (Decl o) = withForeignPtr $! o

-- | Haskell type for @lean_decl@ FFI parameters.
{#pointer lean_decl as DeclPtr -> Decl#}

-- | Haskell type for @lean_decl*@ FFI parameters.
{#pointer *lean_decl as OutDeclPtr -> DeclPtr#}

instance IsLeanValue Decl (Ptr Decl) where
   mkLeanValue = fmap Decl . newForeignPtr lean_decl_del_ptr

foreign import ccall unsafe "&lean_decl_del"
  lean_decl_del_ptr :: FunPtr (DeclPtr -> IO ())

------------------------------------------------------------------------
-- CertDecl declaration

{#pointer lean_cert_decl as CertDecl foreign newtype nocode#}

-- | A Lean certified declaration
newtype CertDecl = CertDecl (ForeignPtr CertDecl)

-- | Function @c2hs@ uses to pass @CertDecl@ values to Lean
withCertDecl :: CertDecl -> (Ptr CertDecl -> IO a) -> IO a
withCertDecl (CertDecl o) = withForeignPtr $! o

-- | Haskell type for @lean_cert_decl@ FFI parameters.
{#pointer lean_cert_decl as CertDeclPtr -> CertDecl#}

-- | Haskell type for @lean_cert_decl*@ FFI parameters.
{#pointer *lean_cert_decl as OutCertDeclPtr -> CertDeclPtr#}

instance IsLeanValue CertDecl (Ptr CertDecl) where
   mkLeanValue = fmap CertDecl . newForeignPtr lean_cert_decl_del_ptr

foreign import ccall unsafe "&lean_cert_decl_del"
  lean_cert_decl_del_ptr :: FunPtr (CertDeclPtr -> IO ())
