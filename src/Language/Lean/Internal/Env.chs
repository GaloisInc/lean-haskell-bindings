{-# LANGUAGE DoAndIfThenElse #-}
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
    -- * Projections
  , envTrustLevel
  , envContainsProofIrrelProp
  , envIsImpredicative
  , envContainsUniv
  , envLookupDecl
  , envIsDescendant
    -- * Operations
  , envForget
  , envFoldDecls
  , envFoldUnivs
    -- * Foreign interface
  , EnvPtr
  , OutEnvPtr
  , tryGetEnv
  , withEnv
  , trustFromUInt
  , trustUInt
  ) where

import Control.Exception (bracket)
import Control.Lens
import Data.IORef
import Foreign
import Foreign.C
import System.IO.Unsafe

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

-- | Function prototype to wrap a Lean function that visits arguments.
type WrapLeanVisitFn p = (p -> IO ()) -> IO (FunPtr (p -> IO ()))

-- | Prototype for function that
type LeanFoldFn s p = s -> FunPtr (p -> IO ()) -> OutExceptionPtr -> IO Bool

runLeanFold :: WrapLeanVisitFn p
            -> (p -> IO a)
            -> LeanFoldFn s p
            ->  Fold s a
runLeanFold wrapFn allocFn foldFn h e = unsafePerformIO $ do
  -- Create reference for storing result, the initial value is pure.
  ref <- newIORef $ coerce $ pure ()

  let g d_ptr = do
        d <- allocFn d_ptr
        modifyIORef' ref $ (<* h d)

  -- Create function pointer for callback.
  bracket (wrapFn g) freeHaskellFunPtr $ \g_ptr -> do
    alloca $ \ex_ptr -> do
      success <- foldFn e g_ptr ex_ptr
      if success then
        readIORef ref
      else
        throwLeanException =<< peek ex_ptr
{-# INLINABLE runLeanFold #-}

------------------------------------------------------------------------
-- Env top-level

-- | Call a C layer function that attempts to allocate a
-- new declaration.
tryGetEnv :: LeanPartialFn EnvPtr -> Env
tryGetEnv mk =
  Env $ tryGetLeanValue lean_env_del_ptr $ mk

foreign import ccall "&lean_env_del"
  lean_env_del_ptr :: FunPtr (EnvPtr -> IO ())

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
stdEnv lvl = tryGetEnv $ lean_env_mk_std lvl

{#fun unsafe lean_env_mk_std
  { trustUInt `TrustLevel'
  , `OutEnvPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create an axiom with name @nm@, universe parameters names
-- @params@, and type @tp@. Note that declartions are universe
-- polymorphic in Lean.
hottEnv :: TrustLevel -> Env
hottEnv lvl = tryGetEnv $ lean_env_mk_hott lvl

{#fun unsafe lean_env_mk_hott
  { trustUInt `TrustLevel'
  , `OutEnvPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Add a new global universe with the given name.
envAddUniv :: Env -> Name -> Env
envAddUniv e u = tryGetEnv $ lean_env_add_univ e u

{#fun unsafe lean_env_add_univ
  { `Env'
  , `Name'
  , `OutEnvPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create a new environment by adding the given certified declaration to the
-- environment.
envAddDecl :: Env -> CertDecl -> Env
envAddDecl e d = tryGetEnv $ lean_env_add e d

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
envReplaceAxiom e d = tryGetEnv $ lean_env_replace e d

{#fun unsafe lean_env_replace
  { `Env'
  , `CertDecl'
  , `OutEnvPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

------------------------------------------------------------------------
-- Env Projections


--  | Return the trust level of the given environment.
{#fun pure unsafe lean_env_trust_level as envTrustLevel
  { `Env' } -> `TrustLevel' trustFromUInt #}

-- | Return @true@ if the given environment has a proof irrelevant Prop such as
-- @Type.{0}@.
{#fun pure unsafe lean_env_proof_irrel as envContainsProofIrrelProp
   { `Env' } -> `Bool' #}

-- | Return @true@ iff in the given environment @Prop@ is impredicative.
{#fun pure unsafe lean_env_impredicative as envIsImpredicative
   { `Env' } -> `Bool' #}

-- |  Return @true@ iff the environment contains a global universe with the name.
{#fun pure unsafe lean_env_contains_univ as envContainsUniv
    { `Env', `Name' } -> `Bool' #}

-- |  Return @true@ iff the environment contains a declaration with the name.
envLookupDecl :: Name -> Env -> Maybe Decl
envLookupDecl nm e =
  if lean_env_contains_decl e nm then
    Just (tryGetDecl $ lean_env_get_decl e nm)
  else
    Nothing

{#fun pure unsafe lean_env_contains_decl
    { `Env', `Name' } -> `Bool' #}

-- |  Return the declaration with the given name in the environment if any.
{#fun unsafe lean_env_get_decl
     { `Env', `Name', `OutDeclPtr', `OutExceptionPtr' } -> `Bool' #}

-- @x `envIsDescendant` y@ return true @x@ is a descendant of @y@, that is, @x@
-- was created by adding declarations to @y@.
{#fun pure unsafe lean_env_is_descendant as envIsDescendant
    { `Env', `Env' } -> `Bool' #}

------------------------------------------------------------------------
-- envForget

-- | Return a new environment, where its "history" has been truncated/forgotten.
-- That is, @envForget x `envIsDescendant y@ will return false for any environment
-- @y@ that is not pointer equal to the result @envForget x@.
envForget :: Env -> Env
envForget x = tryGetEnv $ lean_env_forget x

-- |  Return the declaration with the given name in the environment if any.
{#fun unsafe lean_env_forget
     { `Env', `OutEnvPtr', `OutExceptionPtr' } -> `Bool' #}


------------------------------------------------------------------------
-- foldEnvDecls

------------------------------------------------------------------------
-- foldEnvDecls

-- | A fold over the declratation in the environment.
envFoldDecls :: Fold Env Decl
envFoldDecls = runLeanFold wrapDeclVisitFn allocDecl lean_env_for_each_decl

foreign import ccall "wrapper" wrapDeclVisitFn :: WrapLeanVisitFn DeclPtr

{#fun lean_env_for_each_decl
     { `Env'
     , id `FunPtr (DeclPtr -> IO ())'
     , `OutExceptionPtr'
     } -> `Bool' #}

------------------------------------------------------------------------
-- foldEnvUnivs

-- | Fold over the global universes in the environment.
envFoldUnivs :: Fold Env Name
envFoldUnivs = runLeanFold wrapNameVisitFn allocName lean_env_for_each_univ

foreign import ccall "wrapper" wrapNameVisitFn :: WrapLeanVisitFn NamePtr

{#fun lean_env_for_each_univ
     { `Env'
     , id `FunPtr (NamePtr -> IO ())'
     , `OutExceptionPtr'
     } -> `Bool' #}
