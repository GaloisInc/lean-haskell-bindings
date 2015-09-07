{-|
Module      : Language.Lean.Env
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Operations for working with Lean environments.
-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Language.Lean.Env
  ( Env
  , TrustLevel
  , trustHigh
    -- * Constructing and manipulating environments.
  , standardEnv
  , hottEnv
  , envAddUniv
  , envAddDecl
  , envReplaceAxiom
    -- * Projections
  , envTrustLevel
  , envContainsProofIrrelProp
  , envIsImpredicative
  , envContainsDecl
  , envLookupDecl
  , envContainsUniv
  , envIsDescendant
    -- * Operations
  , envForget
  , envDecls
  , forEnvDecl_
  , envUnivs
  , forEnvUniv_
  ) where

import Control.Exception (bracket)
import Control.Lens
import Control.Monad
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

runLeanFold :: IsLeanValue a p
            => WrapLeanVisitFn p
            -> LeanFoldFn s p
            -> Fold s a
runLeanFold wrapFn foldFn h e = unsafePerformIO $ do
  -- Create reference for storing result, the initial value is pure.
  ref <- newIORef $! (coerce $! pure ())

  let g d_ptr = do
        d <- mkLeanValue d_ptr
        cur_val <- readIORef ref
        let hd = h d
        let chd = coerce hd
        seq hd $ seq chd $ do
        writeIORef ref $! cur_val *> chd

  -- Create function pointer for callback.
  bracket (wrapFn g) freeHaskellFunPtr $ \g_ptr -> do
    alloca $ \ex_ptr -> do
      success <- foldFn e g_ptr ex_ptr
      if success then
        readIORef ref
      else
        throwLeanException =<< peek ex_ptr
{-# INLINABLE runLeanFold #-}

safeRunLeanFold :: IsLeanValue a p
                => WrapLeanVisitFn p
                -> LeanFoldFn s p
                -> (a -> IO ())
                -> (s -> IO ())
safeRunLeanFold wrapFn foldFn f s = do
  let g v = mkLeanValue v >>= f

  -- Create function pointer for callback.
  bracket (wrapFn g) freeHaskellFunPtr $ \g_ptr -> do
    alloca $ \ex_ptr -> do
      success <- foldFn s g_ptr ex_ptr
      unless success $ do
        throwLeanException =<< peek ex_ptr
{-# INLINABLE safeRunLeanFold #-}

------------------------------------------------------------------------
-- Trust level

-- | The level of trust associated with an environment.
newtype TrustLevel = TrustLevel { _trustValue :: Word32 }
  deriving (Eq, Ord, Num, Show)

-- | Create a trust level from a unsigned C integer.
trustFromUInt :: CUInt -> TrustLevel
trustFromUInt = TrustLevel . fromIntegral

-- | Get the trust level as a unsigned C integer.
trustUInt :: TrustLevel -> CUInt
trustUInt (TrustLevel u) = fromIntegral u

-- | Trust level for all macros implemented in Lean.
trustHigh :: TrustLevel
trustHigh = TrustLevel {#const LEAN_TRUST_HIGH#}

------------------------------------------------------------------------
-- Env constructors

-- | Create an empty standard environment with the given trust level.
standardEnv :: TrustLevel -> Env
standardEnv lvl = tryGetLeanValue $ lean_env_mk_std lvl

{#fun unsafe lean_env_mk_std
  { trustUInt `TrustLevel'
  , `OutEnvPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Create an empty hott environment with the given trust level.
hottEnv :: TrustLevel -> Env
hottEnv lvl = tryGetLeanValue $ lean_env_mk_hott lvl

{#fun unsafe lean_env_mk_hott
  { trustUInt `TrustLevel'
  , `OutEnvPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Add a new global universe with the given name.
envAddUniv :: Name -> Env -> Env
envAddUniv u e = tryGetLeanValue $ lean_env_add_univ e u

{#fun unsafe lean_env_add_univ
  { `Env'
  , `Name'
  , `OutEnvPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Adding the given certified declaration to the environment.
envAddDecl :: CertDecl -> Env -> Env
envAddDecl d e = tryGetLeanValue $ lean_env_add e d

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
envReplaceAxiom :: CertDecl -> Env -> Env
envReplaceAxiom d e = tryGetLeanValue $ lean_env_replace e d

{#fun unsafe lean_env_replace
  { `Env'
  , `CertDecl'
  , `OutEnvPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

------------------------------------------------------------------------
-- Env Projections

-- | The trust level of the given environment.
envTrustLevel :: Env -> TrustLevel
envTrustLevel = lean_env_trust_level

{#fun pure unsafe lean_env_trust_level
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
{#fun pure unsafe lean_env_contains_decl as envContainsDecl
    { `Env', `Name' } -> `Bool' #}

-- |  Lookup the declaration with the given name in the environment.
envLookupDecl :: Name -> Env -> Maybe Decl
envLookupDecl nm e =
  if envContainsDecl e nm then
    Just (tryGetLeanValue $ lean_env_get_decl e nm)
  else
    Nothing

-- |  Return the declaration with the given name in the environment if any.
{#fun unsafe lean_env_get_decl
 { `Env', `Name', `OutDeclPtr', `OutExceptionPtr' } -> `Bool' #}

-- | @x `envIsDescendant` y@ return true @x@ is a descendant of @y@, that is, @x@
-- was created by adding declarations to @y@.
envIsDescendant :: Env -> Env -> Bool
envIsDescendant = lean_env_is_descendant

{#fun pure unsafe lean_env_is_descendant
 { `Env', `Env' } -> `Bool' #}

------------------------------------------------------------------------
-- envForget

-- | Return a new environment, where its "history" has been truncated/forgotten.
-- That is, @envForget x `envIsDescendant y@ will return false for any environment
-- @y@ that is not pointer equal to the result @envForget x@.
envForget :: Env -> Env
envForget x = tryGetLeanValue $ lean_env_forget x

-- |  Return the declaration with the given name in the environment if any.
{#fun unsafe lean_env_forget
     { `Env', `OutEnvPtr', `OutExceptionPtr' } -> `Bool' #}

------------------------------------------------------------------------
-- foldEnvDecls

-- | A fold over the declaration in the environment.
envDecls :: Fold Env Decl
envDecls = runLeanFold wrapDeclVisitFn lean_env_for_each_decl

-- | Run an IO action on each declaration in the environment.
forEnvDecl_ :: Env -> (Decl -> IO ()) -> IO ()
forEnvDecl_ e f = safeRunLeanFold wrapDeclVisitFn lean_env_for_each_decl f e

foreign import ccall "wrapper" wrapDeclVisitFn :: WrapLeanVisitFn DeclPtr

{#fun lean_env_for_each_decl
     { `Env'
     , id `FunPtr (DeclPtr -> IO ())'
     , `OutExceptionPtr'
     } -> `Bool' #}

------------------------------------------------------------------------
-- foldEnvUnivs

-- | Fold over the global universes in the environment.
envUnivs :: Fold Env Name
envUnivs = runLeanFold wrapNameVisitFn lean_env_for_each_univ

-- | Run an IO action on each universe in the environment.
forEnvUniv_ :: Env -> (Name -> IO ()) -> IO ()
forEnvUniv_ e f = safeRunLeanFold wrapNameVisitFn lean_env_for_each_univ f e


foreign import ccall "wrapper" wrapNameVisitFn :: WrapLeanVisitFn NamePtr

{#fun lean_env_for_each_univ
     { `Env'
     , id `FunPtr (NamePtr -> IO ())'
     , `OutExceptionPtr'
     } -> `Bool' #}
