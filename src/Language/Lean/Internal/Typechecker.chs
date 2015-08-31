{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Lean.Internal.Typechecker
  ( Typechecker
  , ConstraintSeq
    -- * Foreign exports
  , ConstraintSeqPtr
  , OutConstraintSeqPtr
  , withConstraintSeq
  , TypecheckerPtr
  , OutTypecheckerPtr
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

-- | A constraint Sequence
{#pointer lean_cnstr_seq as ConstraintSeq foreign newtype#}

{#pointer lean_cnstr_seq as ConstraintSeqPtr -> ConstraintSeq#}
{#pointer *lean_cnstr_seq as OutConstraintSeqPtr -> ConstraintSeqPtr #}

instance IsLeanValue ConstraintSeq (Ptr ConstraintSeq) where
  mkLeanValue = fmap ConstraintSeq . newForeignPtr lean_cnstr_seq_del_ptr

foreign import ccall "&lean_cnstr_seq_del"
  lean_cnstr_seq_del_ptr :: FunPtr (ConstraintSeqPtr -> IO ())

------------------------------------------------------------------------
-- Typechecker

-- | A Lean typechecker
{#pointer lean_type_checker as Typechecker foreign newtype#}

{#pointer lean_type_checker as TypecheckerPtr -> Typechecker#}
{#pointer *lean_type_checker as OutTypecheckerPtr -> TypecheckerPtr #}

instance IsLeanValue Typechecker (Ptr Typechecker) where
  mkLeanValue = fmap Typechecker . newForeignPtr lean_type_checker_del_ptr

foreign import ccall "&lean_type_checker_del"
  lean_type_checker_del_ptr :: FunPtr (TypecheckerPtr -> IO ())
