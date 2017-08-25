{-|
Module      : Language.Lean.Inductive
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Operations for creating inductive types and declarations.
-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Trustworthy #-}
module Language.Lean.Inductive
  ( -- * Inductive declarations
    InductiveDecl
  , inductiveDecl
  , inductiveDeclName
  , inductiveDeclUnivParams
  , inductiveDeclNumParams
  , inductiveDeclType
  , inductiveDeclConstructors
    -- * Utility functions
  , recursorName
    -- * Environment operations
  , addInductiveDecl
  , lookupInductiveDecl
  , lookupConstructorInductiveTypeName
  , lookupRecursorInductiveTypeName
  , lookupInductiveTypeNumIndices
  , lookupInductiveTypeNumMinorPremises
  , inductiveTypeHasDepElim
  ) where

import Foreign
import Foreign.C
import Language.Lean.List
import Language.Lean.Internal.Exception.Unsafe

{#import Language.Lean.Internal.Exception#}
{#import Language.Lean.Internal.Expr#}
{#import Language.Lean.Internal.Inductive#}
{#import Language.Lean.Internal.Name#}

--import Language.Lean.Expr

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
-- recursorName

-- | Get the name of the recursor (aka eliminator) associated with a inductive type with
-- given name.
recursorName :: Name -> Name
recursorName n = getLeanValue $ lean_get_recursor_name n

{#fun unsafe lean_get_recursor_name
 { `Name'
 , `OutNamePtr'
 , `OutExceptionPtr'
 }  -> `Bool' #}

------------------------------------------------------------------------
-- Constructing InductiveDecls


-- | Create a new inductive type.
--
-- The remaining inductive datatype arguments are treated as indices.
inductiveDecl :: Name       -- ^ Name of new type
              -> List Name  -- ^ Universe parameter names
              -> Word32     -- ^ Number of parameters
              -> Expr       -- ^ Type of declaration
              -> List Expr  -- ^ Constructor rules.
              -> InductiveDecl
inductiveDecl n ps nparams t cs =
  getLeanValue $ lean_inductive_decl_mk n ps nparams t cs

{#fun unsafe lean_inductive_decl_mk
 { `Name'
 , `ListName'
 , `Word32'
 , `Expr'
 , `ListExpr'
 , `OutInductiveDeclPtr'
 , `OutExceptionPtr'
 } -> `Bool' #}


------------------------------------------------------------------------
-- InductiveDecl projections

-- | Get the name of a inductive type.
inductiveDeclName :: InductiveDecl -> Name
inductiveDeclName d = getLeanValue $ lean_inductive_decl_get_name d

{#fun unsafe lean_inductive_decl_get_name
 { `InductiveDecl'
 , `OutNamePtr'
 , `OutExceptionPtr'
 } -> `Bool' #}

-- | Get the type of a inductive type.
inductiveDeclType :: InductiveDecl -> Expr
inductiveDeclType d = getLeanValue $ lean_inductive_decl_get_type d

{#fun unsafe lean_inductive_decl_get_type
 { `InductiveDecl'
 , `OutExprPtr'
 , `OutExceptionPtr'
 } -> `Bool' #}

-- | Get the list of constructors associated with the given inductive type.
inductiveDeclConstructors :: InductiveDecl -> List Expr
inductiveDeclConstructors d = getLeanValue $ lean_inductive_decl_get_constructors d

{#fun unsafe lean_inductive_decl_get_constructors
 { `InductiveDecl'
 , `OutListExprPtr'
 , `OutExceptionPtr'
 } -> `Bool' #}

-- | Get the list of universe parameter names for the given inductive declaration.
inductiveDeclUnivParams :: InductiveDecl -> List Name
inductiveDeclUnivParams d = getLeanValue $ lean_inductive_decl_get_univ_params d

{#fun lean_inductive_decl_get_univ_params
 { `InductiveDecl', `OutListNamePtr', `OutExceptionPtr' } -> `Bool' #}

-- | Get the number of parameters of the inductive declaration.
inductiveDeclNumParams :: InductiveDecl -> Word32
inductiveDeclNumParams d = getLeanValue $ lean_inductive_decl_get_num_params d

{#fun lean_inductive_decl_get_num_params
 { `InductiveDecl', id `Ptr CUInt', `OutExceptionPtr' } -> `Bool' #}

------------------------------------------------------------------------
-- InductiveDecl operations

-- | Add the inductive declaration to the given environment.
addInductiveDecl :: InductiveDecl -> Env -> Env
addInductiveDecl d e = getLeanValue $ lean_env_add_inductive e d

{#fun lean_env_add_inductive
 { `Env', `InductiveDecl', `OutEnvPtr', `OutExceptionPtr' } -> `Bool' #}

-- | Return the inductive declaration that introduced the type with the given
-- name in the environment (or 'Nothing' if no inductive type by that name exists).
lookupInductiveDecl :: Env -> Name -> Maybe InductiveDecl
lookupInductiveDecl e nm = getLeanMaybeValue $ lean_env_is_inductive_type e nm

{#fun lean_env_is_inductive_type
 { `Env', `Name', `OutInductiveDeclPtr', `OutExceptionPtr' } -> `Bool' #}

-- | If the given name is a constructor in the environment, this returns
-- the name of the associated inductive type.
--
-- If the name is not a constructor, then this returns 'Nothing'.
lookupConstructorInductiveTypeName :: Env -> Name -> Maybe Name
lookupConstructorInductiveTypeName e nm =
  getLeanMaybeValue $ lean_env_is_constructor e nm

{#fun lean_env_is_constructor
 { `Env', `Name', `OutNamePtr', `OutExceptionPtr' } -> `Bool' #}

-- | If the given name is a recursor in the given environment, this returns the
-- name of the associated inductive type.
--
-- If the name is not a recursor, then this returns 'Nothing'.
lookupRecursorInductiveTypeName :: Env -> Name -> Maybe Name
lookupRecursorInductiveTypeName e nm = getLeanMaybeValue $ lean_env_is_recursor e nm

{#fun lean_env_is_recursor
 { `Env', `Name', `OutNamePtr', `OutExceptionPtr' } -> `Bool' #}

-- | Given the name of an inductive type in the environment, this returns
-- the number of indices.
--
-- If the name is not an inductive type in the environment, then this returns
-- 'Nothing'.
lookupInductiveTypeNumIndices :: Env -> Name -> Maybe Word32
lookupInductiveTypeNumIndices e nm =
  getLeanMaybeValue $ lean_env_get_inductive_type_num_indices e nm

{#fun lean_env_get_inductive_type_num_indices
 { `Env', `Name', id `Ptr CUInt', `OutExceptionPtr' } -> `Bool' #}

-- | Given the name of an inductive type in the environment, this returns
-- the number of minor premises for the recursor associated to this type.
--
-- If the name is not an inductive type, then this returns 'Nothing'.
lookupInductiveTypeNumMinorPremises :: Env -> Name -> Maybe Word32
lookupInductiveTypeNumMinorPremises e nm =
  getLeanMaybeValue $ lean_env_get_inductive_type_num_minor_premises e nm

{#fun lean_env_get_inductive_type_num_minor_premises
 { `Env', `Name', id `Ptr CUInt', `OutExceptionPtr' } -> `Bool' #}

-- | Given a name, this returns @True@ if it is the name of an inductive
-- type that supports dependent elimination.
inductiveTypeHasDepElim :: Env -> Name -> Bool
inductiveTypeHasDepElim e nm =
  getLeanValue $ lean_env_get_inductive_type_has_dep_elim e nm

{#fun lean_env_get_inductive_type_has_dep_elim
 { `Env', `Name', id `Ptr CInt', `OutExceptionPtr' } -> `Bool' #}
