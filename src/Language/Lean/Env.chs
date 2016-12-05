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
{-# LANGUAGE Trustworthy #-}
module Language.Lean.Env
  ( Env
  , TrustLevel
  , trustHigh
    -- * Empty environments
  , standardEnv
--  , hottEnv
    -- * Environment information
  , envTrustLevel
--  , envHasProofIrrelevantProp
--  , envIsImpredicative
    -- * Environment universe operations
  , envAddUniv
  , envContainsUniv
  , envUnivs
  , forEnvUniv_
    -- * Environment declaration information
  , envAddCertDecl
  , envReplaceAxiom
  , envContainsDecl
  , envLookupDecl
  , envDecls
  , forEnvDecl_
    -- * Environment containment
  , envIsDescendant
  , envForget
  ) where

import Control.Exception (bracket, throwIO)
import Control.Lens
import Control.Monad
import Data.IORef
import Foreign
import Foreign.C
import System.IO.Unsafe

{#import Language.Lean.Internal.Decl#}
{#import Language.Lean.Internal.Exception#}
import Language.Lean.Internal.Exception.Unsafe
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
  ref <- (newIORef $! (() >$ pure ()))

  let g d_ptr = do
        d <- mkLeanValue d_ptr
        cur_val <- readIORef ref
        let hd = h d
        seq hd $ do
        writeIORef ref $! cur_val <* hd

  -- Create function pointer for callback.
  bracket (wrapFn g) freeHaskellFunPtr $ \g_ptr -> do
    alloca $ \ex_ptr -> do
      success <- foldFn e g_ptr ex_ptr
      if success then
        readIORef ref
      else
        throwIO =<< mkLeanException =<< peek ex_ptr
{-# INLINABLE runLeanFold #-}

safeRunLeanFold :: IsLeanValue a p
                => WrapLeanVisitFn p
                -> LeanFoldFn s p
                -> (a -> IO ())
                -> (s -> IO ())
safeRunLeanFold wrapFn foldFn f s = do
  let g = mkLeanValue >=> f

  -- Create function pointer for callback.
  bracket (wrapFn g) freeHaskellFunPtr $ \g_ptr -> do
    alloca $ \ex_ptr -> do
      success <- foldFn s g_ptr ex_ptr
      unless success $ do
        throwIO =<< mkLeanException =<< peek ex_ptr
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
-- Constructors for empty universes.

-- | Create an empty standard environment with the given trust level.
--
-- The returned an environment is not a descendant of any other environment.
standardEnv :: TrustLevel -> IO Env
standardEnv lvl = allocLeanValue mkLeanException $ lean_env_mk_std lvl

{#fun unsafe lean_env_mk_std
  { trustUInt `TrustLevel'
  , `OutEnvPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{-
-- | Create an empty hott environment with the given trust level.
--
-- The returned an environment is not a descendant of any other environment.
hottEnv :: TrustLevel -> IO Env
hottEnv lvl = allocLeanValue mkLeanException $ lean_env_mk_hott lvl

{#fun unsafe lean_env_mk_hott
  { trustUInt `TrustLevel'
  , `OutEnvPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}
-}

------------------------------------------------------------------------
-- Env information

-- | Return the trust level of the given environment.
{#fun pure unsafe lean_env_trust_level as envTrustLevel
  { `Env' } -> `TrustLevel' trustFromUInt #}

{-
-- | Returns 'True' if all proofs of a proposition in @Prop@ are equivalent.
{#fun pure unsafe lean_env_proof_irrel as envHasProofIrrelevantProp
   { `Env' } -> `Bool' #}

-- | Return whether @Prop@ is impredicative in the environment.
{#fun pure unsafe lean_env_impredicative as envIsImpredicative
   { `Env' } -> `Bool' #}
-}

------------------------------------------------------------------------
-- Env global universe functions

-- | Add a new global universe with the given name.
--
-- This throws a 'LeanException' if the environment already contains a universe
-- level with the given name.
--
-- 'envContainsUniv' can be used to check whether the environment already contains
-- a global universe with the given name.
--
-- The returned an environment is a descendant of the input environment.
envAddUniv :: Name -> Env -> IO Env
envAddUniv u e = allocLeanValue (mkLeanExceptionWithEnv e) $
  lean_env_add_univ e u

{#fun unsafe lean_env_add_univ
  { `Env'
  , `Name'
  , `OutEnvPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- |  Return @true@ iff the environment contains a global universe with the name.
{#fun pure unsafe lean_env_contains_univ as envContainsUniv
    { `Env', `Name' } -> `Bool' #}

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

------------------------------------------------------------------------
-- Env declaration functions

-- | Adding the given certified declaration to the environment.
--
-- This throws a 'LeanException' if the environment is not a descendant
-- of the environment used to certify the declaration originally.
--
-- This throws a 'LeanException' if the environment already contains a
-- declaration with the same name as the certified declaration.
--
-- 'envContainsDecl' can be used to check whether the environment already contains
-- a global declaration with a specific name.
--
-- The returned an environment is a descendant of the input environment.
envAddCertDecl :: CertDecl -> Env -> IO Env
envAddCertDecl d e = allocLeanValue (mkLeanExceptionWithEnv e) $ lean_env_add e d

{#fun unsafe lean_env_add
  { `Env'
  , `CertDecl'
  , `OutEnvPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- |  Replace the axiom that has the name of the given certified declaration
-- with the certified declaration.
--
-- This procedure throws a LeanException if
--
--  * The certified declaration is not a theorem.
--  * The theorem was certified in an environment which is not an ancestor of the environment.
--  * The environment does not contain an axiom with the given name.
--
-- The returned environment is a descendant of the input environment.
envReplaceAxiom :: CertDecl -> Env -> IO Env
envReplaceAxiom d e = allocLeanValue (mkLeanExceptionWithEnv e) $ lean_env_replace e d

{#fun unsafe lean_env_replace
  { `Env'
  , `CertDecl'
  , `OutEnvPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- |  Return @true@ iff the environment contains a declaration with the name.
{#fun pure unsafe lean_env_contains_decl as envContainsDecl
    { `Env', `Name' } -> `Bool' #}

-- |  Lookup the declaration with the given name in the environment.
envLookupDecl :: Name -> Env -> Maybe Decl
envLookupDecl nm e =
  if envContainsDecl e nm then
    Just (getLeanValue $ lean_env_get_decl e nm)
  else
    Nothing

-- |  Return the declaration with the given name in the environment if any.
{#fun unsafe lean_env_get_decl
 { `Env', `Name', `OutDeclPtr', `OutExceptionPtr' } -> `Bool' #}

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
-- Environment containment

-- | @x `'envIsDescendant'` y@ return true @x@ is a descendant of @y@, that is, @x@
-- was created by adding declarations to @y@.
envIsDescendant :: Env -> Env -> Bool
envIsDescendant = lean_env_is_descendant

{#fun pure unsafe lean_env_is_descendant
 { `Env', `Env' } -> `Bool' #}

-- | Return a new environment, where its "history" has been truncated/forgotten.
-- That is, @envForget x@ will return an environment @y@ that is only a descendant of
-- itself.
envForget :: Env -> IO Env
envForget x = allocLeanValue (mkLeanExceptionWithEnv x) $ lean_env_forget x

-- |  Return the declaration with the given name in the environment if any.
{#fun unsafe lean_env_forget
     { `Env', `OutEnvPtr', `OutExceptionPtr' } -> `Bool' #}
