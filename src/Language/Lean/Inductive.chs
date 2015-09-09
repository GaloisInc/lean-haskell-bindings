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
  ( -- * Inductive type
    InductiveType
  , inductiveType
  , inductiveTypeName
  , inductiveTypeType
  , inductiveTypeConstructors
  , recursorName
    -- * Inductive declarations
  , InductiveDecl
  , inductiveDecl
  , inductiveDeclUnivParams
  , inductiveDeclNumParams
  , inductiveDeclTypes
    -- * Environment operations
  , addInductiveDecl
  , lookupInductiveDecl
  , envIsConstructor
  , envIsRecursor
  , envInductiveTypeNumIndices
  , envInductiveTypeNumMinorPremises
  , envInductiveTypeNumTypeFormers
  , envInductiveTypeHasDepElim
  ) where

import Foreign
import Foreign.C
import Language.Lean.List

{#import Language.Lean.Internal.Decl#}
{#import Language.Lean.Internal.Exception#}
import Language.Lean.Internal.Exception.Unsafe
{#import Language.Lean.Internal.Expr#}
{#import Language.Lean.Internal.Inductive#}
{#import Language.Lean.Internal.Name#}

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
-- Constructing InductiveType

-- | Creates an inductive type
inductiveType :: Name -- ^ Name of the inductive type
              -> Expr -- ^ Type of the inductive type
              -> List Expr -- ^ Constructors (must be a list of local constants)
              -> InductiveType
inductiveType n t cs = tryGetLeanValue $ lean_inductive_type_mk n t cs

{#fun unsafe lean_inductive_type_mk
 { `Name'
 , `Expr'
 , `ListExpr'
 , `OutInductiveTypePtr'
 , `OutExceptionPtr'
 } -> `Bool' #}

------------------------------------------------------------------------
-- Inductive Types

-- | Get the name of a inductive type.
inductiveTypeName :: InductiveType -> Name
inductiveTypeName tp = tryGetLeanValue $ lean_inductive_type_get_name tp

{#fun unsafe lean_inductive_type_get_name
 { `InductiveType'
 , `OutNamePtr'
 , `OutExceptionPtr'
 } -> `Bool' #}

-- | Get the type of a inductive type.
inductiveTypeType :: InductiveType -> Expr
inductiveTypeType tp = tryGetLeanValue $ lean_inductive_type_get_type tp

{#fun unsafe lean_inductive_type_get_type
 { `InductiveType'
 , `OutExprPtr'
 , `OutExceptionPtr'
 } -> `Bool' #}

-- | Get the list of constructors associated with the given inductive type.
inductiveTypeConstructors :: InductiveType -> List Expr
inductiveTypeConstructors tp = tryGetLeanValue $ lean_inductive_type_get_constructors tp

{#fun unsafe lean_inductive_type_get_constructors
 { `InductiveType'
 , `OutListExprPtr'
 , `OutExceptionPtr'
 } -> `Bool' #}

------------------------------------------------------------------------
-- recursorName

-- | Get the name of the recursor (aka eliminator) associated with a inductive type with
-- given name.
recursorName :: Name -> Name
recursorName n = tryGetLeanValue $ lean_get_recursor_name n

{#fun unsafe lean_get_recursor_name
 { `Name'
 , `OutNamePtr'
 , `OutExceptionPtr'
 }  -> `Bool' #}

------------------------------------------------------------------------
-- Constructing InductiveDecls

-- | A inductive datatype declaration
--
-- The remaining inductive datatype arguments are treated as indices.
inductiveDecl :: List Name -- ^ Universe parameters
              -> Word32 -- ^ Number of parameters
              -> List InductiveType -- ^ List of inductive types
              -> InductiveDecl
inductiveDecl ps n types = tryGetLeanValue $ lean_inductive_decl_mk ps n types

{#fun unsafe lean_inductive_decl_mk
 { `ListName'
 , `Word32'
 , `ListInductiveType'
 , `OutInductiveDeclPtr'
 , `OutExceptionPtr'
 } -> `Bool' #}

------------------------------------------------------------------------
-- InductiveDecl projections

-- | Get the list of universe parameter names for the given inductive declaration.
inductiveDeclUnivParams :: InductiveDecl -> List Name
inductiveDeclUnivParams d = tryGetLeanValue $ lean_inductive_decl_get_univ_params d

{#fun lean_inductive_decl_get_univ_params
 { `InductiveDecl', `OutListNamePtr', `OutExceptionPtr' } -> `Bool' #}

-- | Get the number of parameters shared by the inductive types in the
-- declaration.
inductiveDeclNumParams :: InductiveDecl -> Word32
inductiveDeclNumParams d = tryGetLeanValue $ lean_inductive_decl_get_num_params d

{#fun lean_inductive_decl_get_num_params
 { `InductiveDecl', id `Ptr CUInt', `OutExceptionPtr' } -> `Bool' #}

-- | Get  the list of inductive types in the inductive declaration
inductiveDeclTypes :: InductiveDecl -> List InductiveType
inductiveDeclTypes d = tryGetLeanValue $ lean_inductive_decl_get_types d

{#fun lean_inductive_decl_get_types
 { `InductiveDecl', `OutListInductiveTypePtr', `OutExceptionPtr' } -> `Bool' #}

------------------------------------------------------------------------
-- InductiveDecl operations

-- | Add the inductive declaration to the given environment.
addInductiveDecl :: InductiveDecl -> Env -> Env
addInductiveDecl d e = tryGetLeanValue $ lean_env_add_inductive e d

{#fun lean_env_add_inductive
 { `Env', `InductiveDecl', `OutEnvPtr', `OutExceptionPtr' } -> `Bool' #}

-- | Return the inductive declaration associated with the name in the
-- environment if one is defined.
lookupInductiveDecl :: Env -> Name -> Maybe InductiveDecl
lookupInductiveDecl e nm = tryGetLeanMaybeValue $ lean_env_is_inductive_type e nm

{#fun lean_env_is_inductive_type
 { `Env', `Name', `OutInductiveDeclPtr', `OutExceptionPtr' } -> `Bool' #}

-- | Return the name of of the associated inductive type in the environment
-- given a constructor in the given environment.
envIsConstructor :: Env -> Name -> Name
envIsConstructor e nm = tryGetLeanValue $ lean_env_is_constructor e nm

{#fun lean_env_is_constructor
 { `Env', `Name', `OutNamePtr', `OutExceptionPtr' } -> `Bool' #}

-- | Given the name of a recursor in the environment, this returns the name
-- of the associated inductive type.
envIsRecursor :: Env -> Name -> Name
envIsRecursor e nm = tryGetLeanValue $ lean_env_is_recursor e nm

{#fun lean_env_is_recursor
 { `Env', `Name', `OutNamePtr', `OutExceptionPtr' } -> `Bool' #}

-- | Given the name of an inductive type in the environment, this returns
-- the number of indices.
envInductiveTypeNumIndices :: Env -> Name -> Word32
envInductiveTypeNumIndices e nm =
  tryGetLeanValue $ lean_env_get_inductive_type_num_indices e nm

{#fun lean_env_get_inductive_type_num_indices
 { `Env', `Name', id `Ptr CUInt', `OutExceptionPtr' } -> `Bool' #}

-- | Given the name of an inductive type in the environment, this returns
-- the number of minor premises.
envInductiveTypeNumMinorPremises :: Env -> Name -> Word32
envInductiveTypeNumMinorPremises e nm =
  tryGetLeanValue $ lean_env_get_inductive_type_num_minor_premises e nm

{#fun lean_env_get_inductive_type_num_minor_premises
 { `Env', `Name', id `Ptr CUInt', `OutExceptionPtr' } -> `Bool' #}

-- | Given the name of an inductive type in the environment, this returns
-- the number of type formers.
envInductiveTypeNumTypeFormers :: Env -> Name -> Word32
envInductiveTypeNumTypeFormers e nm =
  tryGetLeanValue $ lean_env_get_inductive_type_num_type_formers e nm

{#fun lean_env_get_inductive_type_num_type_formers
 { `Env', `Name', id `Ptr CUInt', `OutExceptionPtr' } -> `Bool' #}

-- | Given the name of an inductive type in the environment, this returns true
-- if the inductive type supports dependent elimination.
envInductiveTypeHasDepElim :: Env -> Name -> Bool
envInductiveTypeHasDepElim e nm =
  tryGetLeanValue $ lean_env_get_inductive_type_has_dep_elim e nm

{#fun lean_env_get_inductive_type_has_dep_elim
 { `Env', `Name', id `Ptr CInt', `OutExceptionPtr' } -> `Bool' #}
