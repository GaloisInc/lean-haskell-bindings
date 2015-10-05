{-|
Module      : Language.Lean.Internal.Typechecker
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Internal declarations for ConstraintSeq and Typechecker.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}
module Language.Lean.Internal.Typechecker
  ( Typechecker
  , ConstraintSeq
    -- * Internal exports
  , ConstraintSeqPtr
  , OutConstraintSeqPtr
  , withConstraintSeq
  , TypecheckerPtr
  , OutTypecheckerPtr
  , mkTypechecker
  , typecheckerEnv
  , withTypechecker
  ) where

import Foreign

{#import Language.Lean.Internal.Exception #}

#include "lean_bool.h"
#include "lean_macros.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"
#include "lean_expr.h"
#include "lean_decl.h"
#include "lean_env.h"
#include "lean_type_checker.h"

------------------------------------------------------------------------
-- Constraint Sequence

{#pointer lean_cnstr_seq as ConstraintSeq foreign newtype nocode#}

-- | A sequence of constraints
newtype ConstraintSeq = ConstraintSeq (ForeignPtr ConstraintSeq)

-- | Get access to @lean_cnstr_seq@ within IO action.
withConstraintSeq :: ConstraintSeq -> (Ptr ConstraintSeq -> IO a) -> IO a
withConstraintSeq (ConstraintSeq o) = withForeignPtr $! o

-- | Pointer to @lean_cnstr_seq@ for inputs to Lean functions.
{#pointer lean_cnstr_seq as ConstraintSeqPtr -> ConstraintSeq#}
-- | Pointer to @lean_cnstr_seq@ for outputs from Lean functions.
{#pointer *lean_cnstr_seq as OutConstraintSeqPtr -> ConstraintSeqPtr #}

instance IsLeanValue ConstraintSeq (Ptr ConstraintSeq) where
  mkLeanValue = fmap ConstraintSeq . newForeignPtr lean_cnstr_seq_del_ptr

foreign import ccall unsafe "&lean_cnstr_seq_del"
  lean_cnstr_seq_del_ptr :: FunPtr (ConstraintSeqPtr -> IO ())

------------------------------------------------------------------------
-- Typechecker

-- | A Lean typechecker
data Typechecker = Typechecker !Env !(ForeignPtr Typechecker)

-- | Return the environment associated with a typechecker.
typecheckerEnv :: Typechecker -> Env
typecheckerEnv (Typechecker e _) = e

-- | Function @c2hs@ uses to pass @Typechecker@ values to Lean
withTypechecker :: Typechecker -> (Ptr Typechecker -> IO a) -> IO a
withTypechecker (Typechecker _ o) = withForeignPtr $! o

{#pointer lean_type_checker as Typechecker foreign newtype nocode#}

-- | Haskell type for @lean_type_checker@ FFI parameters.
{#pointer lean_type_checker as TypecheckerPtr -> Typechecker#}
-- | Haskell type for @lean_type_checker*@ FFI parameters.
{#pointer *lean_type_checker as OutTypecheckerPtr -> TypecheckerPtr #}

-- | Create a typechecker for the given environment
mkTypechecker :: Env -> Ptr Typechecker -> IO Typechecker
mkTypechecker e p = Typechecker e <$> newForeignPtr lean_type_checker_del_ptr p

foreign import ccall unsafe "&lean_type_checker_del"
  lean_type_checker_del_ptr :: FunPtr (TypecheckerPtr -> IO ())
