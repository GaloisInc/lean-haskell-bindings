{-|
Module      : Language.Lean.Inductive
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Internal declarations for inductive types and declarations.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK not-home #-}
module Language.Lean.Internal.Inductive
  ( InductiveDecl
  , InductiveDeclPtr
  , OutInductiveDeclPtr
  , withInductiveDecl
  ) where

import Control.Lens (toListOf)
import Foreign
import Foreign.C
import Language.Lean.List

{#import Language.Lean.Internal.Exception#}
import Language.Lean.Internal.Exception.Unsafe

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"
#include "lean_expr.h"
#include "lean_decl.h"
#include "lean_inductive.h"

------------------------------------------------------------------------
-- InductiveDecl declarations

{#pointer  lean_inductive_decl as InductiveDecl foreign newtype nocode#}

-- | An inductive declaration.
--
-- A single inductive declaration may define one or more Lean inductive
-- types.  The inductive types must have the same parameters.
newtype InductiveDecl = InductiveDecl (ForeignPtr InductiveDecl)

-- | Access raw @lean_inductive_decl@ within an 'IO' action.
withInductiveDecl :: InductiveDecl -> (Ptr InductiveDecl -> IO a) -> IO a
withInductiveDecl (InductiveDecl o) = withForeignPtr $! o

-- | Haskell type for @lean_inductive_decl@ FFI parameters.
{#pointer lean_inductive_decl as InductiveDeclPtr -> InductiveDecl #}
-- | Haskell type for @lean_inductive_decl*@ FFI parameters.
{#pointer *lean_inductive_decl as OutInductiveDeclPtr -> InductiveDeclPtr #}

instance IsLeanValue InductiveDecl (Ptr InductiveDecl) where
  mkLeanValue = fmap InductiveDecl . newForeignPtr lean_inductive_decl_del_ptr

foreign import ccall unsafe "&lean_inductive_decl_del"
  lean_inductive_decl_del_ptr :: FunPtr (InductiveDeclPtr -> IO ())
