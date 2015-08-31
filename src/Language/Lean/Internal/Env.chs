{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Language.Lean.Internal.Env
  ( Env
  , TrustLevel
  , trustHigh
    -- * Constructing and manipulating environments.
  , stdEnv
  , hottEnv
  , envAddUniv
  , envAddDecl
  , envReplaceAxiom
    -- * Foreign interface
  , EnvPtr
  , OutEnvPtr
  , tryGetLeanValue
  , withEnv
  , trustFromUInt
  , trustUInt
  ) where

import Foreign
import Foreign.C

{#import Language.Lean.Internal.Decl#}
{#import Language.Lean.Internal.Exception#}
{#import Language.Lean.Internal.Name#}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"
#include "lean_expr.h"
#include "lean_decl.h"
#include "lean_env.h"

------------------------------------------------------------------------
-- Trust level

-- | The level of trust associated with an environment.
newtype TrustLevel = TrustLevel { _trustValue :: Word32 }
  deriving (Eq, Ord, Num, Show)

trustFromUInt :: CUInt -> TrustLevel
trustFromUInt = TrustLevel . fromIntegral

trustUInt :: TrustLevel -> CUInt
trustUInt (TrustLevel u) = fromIntegral u

-- | Trust level for all macros implemented in LEan.
trustHigh :: TrustLevel
trustHigh = TrustLevel {#const LEAN_TRUST_HIGH#}

------------------------------------------------------------------------
-- Env constructors

-- | Create an axiom with name @nm@, universe parameters names
-- @params@, and type @tp@. Note that declartions are universe
-- polymorphic in Lean.
stdEnv :: TrustLevel -> Env
stdEnv lvl = tryGetLeanValue $ lean_env_mk_std lvl

{#fun unsafe lean_env_mk_std
  { trustUInt `TrustLevel'
  , `OutEnvPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create an axiom with name @nm@, universe parameters names
-- @params@, and type @tp@. Note that declartions are universe
-- polymorphic in Lean.
hottEnv :: TrustLevel -> Env
hottEnv lvl = tryGetLeanValue $ lean_env_mk_hott lvl

{#fun unsafe lean_env_mk_hott
  { trustUInt `TrustLevel'
  , `OutEnvPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Add a new global universe with the given name.
envAddUniv :: Env -> Name -> Env
envAddUniv e u = tryGetLeanValue $ lean_env_add_univ e u

{#fun unsafe lean_env_add_univ
  { `Env'
  , `Name'
  , `OutEnvPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a new environment by adding the given certified declaration to the
-- environment.
envAddDecl :: Env -> CertDecl -> Env
envAddDecl e d = tryGetLeanValue $ lean_env_add e d

{#fun unsafe lean_env_add
  { `Env'
  , `CertDecl'
  , `OutEnvPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}


-- |  Replace the axiom that has the name of the given certified declaration with the
-- certified declaration.
--
-- This procedure throws an exception if:
--  * The theorem was certified in an environment which is not an ancestor of the environment.
--  * The environment does not contain an axiom with the given name.
envReplaceAxiom :: Env -> CertDecl -> Env
envReplaceAxiom e d = tryGetLeanValue $ lean_env_replace e d

{#fun unsafe lean_env_replace
  { `Env'
  , `CertDecl'
  , `OutEnvPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}
