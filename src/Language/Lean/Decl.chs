{-|
Module      : Language.Lean.Decl
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Operations for working with Lean declarations.
-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Trustworthy #-}
module Language.Lean.Decl
  ( Decl
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
  , certify
  , tryCertify
  ) where

import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import Language.Lean.List

{#import Language.Lean.Internal.Decl#}
{#import Language.Lean.Internal.Exception#}
import Language.Lean.Internal.Exception.Unsafe
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

-- | Create an axiom.
axiom :: Name -- ^ Name of axiom
      -> List Name -- ^ Universe parameters
      -> Expr -- ^ Type of axiom
      -> Decl
axiom nm params tp = getLeanValue $ lean_decl_mk_axiom nm params tp

{#fun unsafe lean_decl_mk_axiom
  { `Name'
  , `ListName'
  , `Expr'
  , `OutDeclPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a constant.
--
-- Constants and axioms in Lean are essentially the same thing.
constant :: Name -- ^ Name of constant
         -> List Name -- ^ Universe parameters
         -> Expr -- ^ Type of constant
         -> Decl
constant nm params tp = getLeanValue $ lean_decl_mk_const nm params tp

{#fun unsafe lean_decl_mk_const
  { `Name'
  , `ListName'
  , `Expr'
  , `OutDeclPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a definition with an explicit definitional height
definition :: Name -- ^ Name of the definition
           -> List Name -- ^ Universe parameters for defintion
           -> Expr -- ^ Type of definition
           -> Expr -- ^ Value of definition
           -> Word32 -- ^ Definitional height
           -> Bool -- ^ Flag that indicates if definition should be lazily unfolded
           -> Decl
definition nm params tp v h o = getLeanValue $ lean_decl_mk_def nm params tp v h o

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

-- | Create a definition with a definitional height
-- computed from the environment.
--
-- The definitional height is computed using information from the environment.
definitionWith :: Env  -- ^ The environment
               -> Name -- ^ Name of the definition
               -> List Name -- ^ Universe parameters for defintion
               -> Expr -- ^ Type of definition
               -> Expr -- ^ Value of definition
               -> Bool -- ^ Flag that indicates if definition should be lazily unfolded
               -> Decl
definitionWith e nm params tp v o =
  getLeanValue $ lean_decl_mk_def_with e nm params tp v o

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

-- | Creates a theorem with an explicit definitional height.
--
-- Theorems and definitions are essentially the same thing in Lean, except in
-- the way the normalizer treats them. The normalizer will only unfold theroem
-- if there is nothing else to be done when checking whether two terms are
-- definitionally equal or not.
theorem :: Name      -- ^ Name of the theorem
        -> List Name -- ^ Universe parameters for theorem
        -> Expr      -- ^ Type of the theorem
        -> Expr      -- ^ Proof of the theorem
        -> Word32    -- ^ Definitional height
        -> Decl
theorem nm params tp v h = getLeanValue $ lean_decl_mk_thm nm params tp v h

{#fun unsafe lean_decl_mk_thm
  { `Name'
  , `ListName'
  , `Expr'
  , `Expr'
  , `Word32'
  , `OutDeclPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | 'theoremWith' creates a theorem that is relative to an environment.
--
-- Theorems and definitions are essentially the same thing in Lean, except in
-- the way the normalizer treats them. The normalizer will only unfold theroem
-- if there is nothing else to be done when checking whether two terms are
-- definitionally equal or not.
--
-- The definitional height is computed from environment.
theoremWith :: Env       -- ^ The environment
            -> Name      -- ^ The name of the theorem
            -> List Name -- ^ Universe parameters for theorem
            -> Expr      -- ^ Type of the theorem
            -> Expr      -- ^ Proof of the theorem
            -> Decl
theoremWith e nm params tp v = getLeanValue $ lean_decl_mk_thm_with e nm params tp v

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
declName d = getLeanValue $ lean_decl_get_name d

{#fun unsafe lean_decl_get_name
  { `Decl', `OutNamePtr', `OutExceptionPtr' } -> `Bool' #}

-- | The list of universe params for a declaration.
declUnivParams :: Decl -> List Name
declUnivParams d = getLeanValue $ lean_decl_get_univ_params d

{#fun unsafe lean_decl_get_univ_params
  { `Decl', `OutListNamePtr', `OutExceptionPtr' } -> `Bool' #}

-- | The type of a declaration.
declType :: Decl -> Expr
declType d = getLeanValue $ lean_decl_get_type d

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
    -- | A theorem with the associated value and definitional height.
  | Theorem Expr Word32
 deriving (Eq, Show)

-- | Return information about a declaration.
declView :: Decl -> DeclView
declView x =
  case lean_decl_get_kind x of
    LEAN_DECL_CONST -> Constant
    LEAN_DECL_AXIOM -> Axiom
    LEAN_DECL_DEF ->
      Definition (getLeanValue $ lean_decl_get_value x)
                 (getLeanValue $ lean_decl_get_height x)
                 (getLeanValue $ lean_decl_get_conv_opt x)
    LEAN_DECL_THM ->
      Theorem (getLeanValue $ lean_decl_get_value x)
              (getLeanValue $ lean_decl_get_height x)

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
--
-- Throws a `LeanException` if the certification fails
certify :: Env -> Decl ->  CertDecl
certify e d = getLeanValue $ lean_decl_check e d

-- | Tries to creates a cerified declaration by type checking it
-- within a given environment.
--
-- Returns either an exception or the certified declaration.
tryCertify :: Env -> Decl -> Either LeanException CertDecl
tryCertify e d = tryGetLeanValue (mkLeanExceptionWithEnv e emptyOptions) $
  lean_decl_check e d

{#fun unsafe lean_decl_check
  { `Env', `Decl', `OutCertDeclPtr', `OutExceptionPtr' } -> `Bool' #}
