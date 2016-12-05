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
--  , ConstraintSeq
  , typechecker
--  , inferType
--  , tryInferType
--  , checkType
--  , tryCheckType
--  , whnf
--  , tryWhnf
--  , isDefEq
--  , tryIsDefEq
  ) where

--import Foreign
--import Foreign.C
import System.IO.Unsafe

{#import Language.Lean.Internal.Exception#}
--{#import Language.Lean.Internal.Expr#}
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
-- Utilities

-- | A lean partial function is a function that returns a value of type @a@, but
-- may fail.
-- type LeanFn2 a b = (Ptr a -> Ptr b -> LeanAction)

{-
-- | This runs a partial lean function that returns two values in separate
-- pointers.
tryGetLeanPair :: (IsLeanValue a p, IsLeanValue b q)
               => LeanExceptionFn
               -> LeanFn2 p q
               -> Either LeanException (a,b)
tryGetLeanPair except_fn alloc_fn = unsafePerformIO $ do
  alloca $ \p_ptr -> do
    alloca $ \q_ptr -> do
      res <- tryRunLeanAction $ alloc_fn p_ptr q_ptr
      case res of
        Nothing -> do
          p <- mkLeanValue =<< peek p_ptr
          q <- mkLeanValue =<< peek q_ptr
          let pair = seq p $ seq q $ (p,q)
          return $! (seq pair $ Right pair)
        Just p -> Left <$> except_fn p
-}

------------------------------------------------------------------------
-- Typechecker constructor

-- | Create a type checker object for the given environment.
typechecker :: Env -> Typechecker
typechecker e = unsafePerformIO $ do
  mkTypechecker e =<< runLeanFn (mkLeanExceptionWithEnv e) (lean_type_checker_mk e)

{#fun unsafe lean_type_checker_mk
 { `Env', `OutTypecheckerPtr', `OutExceptionPtr' } -> `Bool' #}

------------------------------------------------------------------------
-- Typechecker operations

{-

-- | @inferType t e@ infers the type of @e@ using @t@.
-- This returns the type and any constraints generated.
--
-- The expression @e@ must not contain any free variables (subexpressions with
-- type @ExprVar@).
--
-- This may throw a 'LeanException' if the expression is not well-formed.
inferType :: Typechecker -> Expr -> (Expr, ConstraintSeq)
inferType t e = getPartial $ tryInferType t e

-- | @tryInferType t e@ attempts to infers the type of @e@ using @t@.
-- This returns the type and any constraints generated.
--
-- The expression @e@ should not contain any free variables (subexpressions with
-- type @ExprVar@).
--
-- This version allows the exception to be pattern matched against.
tryInferType :: Typechecker -> Expr -> Either LeanException (Expr, ConstraintSeq)
tryInferType t e = tryGetLeanPair e_fn $ lean_type_checker_infer t e
  where e_fn = mkLeanExceptionWithEnv (typecheckerEnv t)

{#fun unsafe lean_type_checker_infer
     { `Typechecker'
     , `Expr'
     , `OutExprPtr'
     , `OutConstraintSeqPtr'
     , `OutExceptionPtr'
     } -> `Bool' #}

-- | @checkType t e@ checks and infers the type of @e@ using @t@.
-- This returns the type and any constraints generated.
--
-- The expression @e@ must not contain any free variables (subexpressions with
-- type @ExprVar@).
--
-- This may throw a 'LeanException' if the expression is not well-formed.
checkType :: Typechecker -> Expr -> (Expr, ConstraintSeq)
checkType t e = getPartial $ tryCheckType t e

-- | @checkType t e@ checks and infers the type of @e@ using @t@.
-- This returns the type and any constraints generated.
--
-- The expression @e@ must not contain any free variables (subexpressions with
-- type @ExprVar@).
--
-- This version allows the exception to be pattern matched against.
tryCheckType :: Typechecker -> Expr -> Either LeanException (Expr, ConstraintSeq)
tryCheckType t e = tryGetLeanPair e_fn $ lean_type_checker_check t e
  where e_fn = mkLeanExceptionWithEnv (typecheckerEnv t)

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
--
-- This may throw a 'LeanException' if the expression is not well-formed.
whnf :: Typechecker -> Expr -> (Expr, ConstraintSeq)
whnf t e = getPartial $ tryWhnf t e

-- | @whnf t e@ computes the weak-head-normal-form of @e@ using @t@, returning the
-- form and any generated constraints.
--
-- The expression @e@ must not contain any free variables (subexpressions with
-- type @ExprVar@).
tryWhnf :: Typechecker -> Expr -> Either LeanException (Expr, ConstraintSeq)
tryWhnf t e = tryGetLeanPair e_fn $ lean_type_checker_whnf t e
  where e_fn = mkLeanExceptionWithEnv (typecheckerEnv t)

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
--
-- This may throw a 'LeanException' if either expression is not well-formed.
isDefEq :: Typechecker -> Expr -> Expr -> (Bool, ConstraintSeq)
isDefEq t e1 e2 = getPartial $ tryIsDefEq t e1 e2

-- | @is_def_eq t e1 e2@ returns @True@  iff @e1@ and @e2@ are definitionally equal along
-- with any generated constraints.
--
-- The expressions @e1@ and @e2@ must not contain any free variables
-- (subexpressions with type @ExprVar@).
tryIsDefEq :: Typechecker -> Expr -> Expr -> Either LeanException (Bool, ConstraintSeq)
tryIsDefEq t e1 e2 = tryGetLeanPair e_fn $ lean_type_checker_is_def_eq t e1 e2
  where e_fn = mkLeanExceptionWithEnv (typecheckerEnv t)

{#fun unsafe lean_type_checker_is_def_eq
     { `Typechecker'
     , `Expr'
     , `Expr'
     , id `Ptr CInt'
     , `OutConstraintSeqPtr'
     , `OutExceptionPtr'
     } -> `Bool' #}
-}
