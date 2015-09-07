{-|
Module      : Language.Lean.Typechecker
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Interface to Lean typechecker
-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Trustworthy #-}
module Language.Lean.Typechecker
  ( Typechecker
  , ConstraintSeq
  , typechecker
  , inferType
  , checkType
  , whnf
  , isDefEq
  ) where

import Foreign
import Foreign.C
import System.IO.Unsafe

{#import Language.Lean.Internal.Decl#}
{#import Language.Lean.Internal.Exception#}
import Language.Lean.Internal.Exception.Unsafe
{#import Language.Lean.Internal.Expr#}
{#import Language.Lean.Internal.Typechecker#}

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
-- Typechecker constructor

-- | Create a type checker object for the given environment.
typechecker :: Env -> Typechecker
typechecker e = tryGetLeanValue $ lean_type_checker_mk e

{#fun unsafe lean_type_checker_mk
     { `Env', `OutTypecheckerPtr', `OutExceptionPtr' } -> `Bool' #}

------------------------------------------------------------------------
-- Typechecker operations

-- | A lean partial function is a function that returns a value of type @a@, but
-- may fail.
type LeanPartialFn2 a b = (Ptr a -> Ptr b -> LeanPartialAction)

-- | @inferType t e@ infers the type of @e@ using @t@.
-- This returns the type and any constraints generated.
--
-- The expression @e@ must not contain any free variables (subexpressions with
-- type @ExprVar@.
tryGetLeanPair :: (IsLeanValue a p, IsLeanValue b q)
               => LeanPartialFn2 p q
               -> (a,b)
tryGetLeanPair alloc_fn = unsafePerformIO $ do
  alloca $ \p_ptr -> do
    alloca $ \q_ptr -> do
      runLeanPartialAction $ alloc_fn p_ptr q_ptr
      p <- mkLeanValue =<< peek p_ptr
      q <- mkLeanValue =<< peek q_ptr
      seq p $ seq q $ (return $! (p,q))

-- | @inferType t e@ infers the type of @e@ using @t@.
-- This returns the type and any constraints generated.
--
-- The expression @e@ must not contain any free variables (subexpressions with
-- type @ExprVar@).
inferType :: Typechecker -> Expr -> (Expr, ConstraintSeq)
inferType t e = tryGetLeanPair $ lean_type_checker_infer t e

{#fun unsafe lean_type_checker_infer
     { `Typechecker'
     , `Expr'
     , `OutExprPtr'
     , `OutConstraintSeqPtr'
     , `OutExceptionPtr'
     } -> `Bool' #}

-- | @inferType t e@ checks and infers the type of @e@ using @t@.
-- This returns the type and any constraints generated.
--
-- The expression @e@ must not contain any free variables (subexpressions with
-- type @ExprVar@).
checkType :: Typechecker -> Expr -> (Expr, ConstraintSeq)
checkType t e = tryGetLeanPair $ lean_type_checker_check t e

{#fun unsafe lean_type_checker_check
     { `Typechecker'
     , `Expr'
     , `OutExprPtr'
     , `OutConstraintSeqPtr'
     , `OutExceptionPtr'
     } -> `Bool' #}

-- | @whnf t e@ computes the weak-head-normal-form of @e@ using @t@, returning the
-- form and any generated constraints.
--
-- The expression @e@ must not contain any free variables (subexpressions with
-- type @ExprVar@).
whnf :: Typechecker -> Expr -> (Expr, ConstraintSeq)
whnf t e = tryGetLeanPair $ lean_type_checker_whnf t e

{#fun unsafe lean_type_checker_whnf
     { `Typechecker'
     , `Expr'
     , `OutExprPtr'
     , `OutConstraintSeqPtr'
     , `OutExceptionPtr'
     } -> `Bool' #}

-- | @is_def_eq t e1 e2@ returns @True@  iff @e1@ and @e2@ are definitionally equal along
-- with any generated constraints.
--
-- The expressions @e1@ and @e2@ must not contain any free variables
-- (subexpressions with type @ExprVar@).
isDefEq :: Typechecker -> Expr -> Expr -> (Bool, ConstraintSeq)
isDefEq t e1 e2 = tryGetLeanPair $ lean_type_checker_is_def_eq t e1 e2

{#fun unsafe lean_type_checker_is_def_eq
     { `Typechecker'
     , `Expr'
     , `Expr'
     , id `Ptr CInt'
     , `OutConstraintSeqPtr'
     , `OutExceptionPtr'
     } -> `Bool' #}
