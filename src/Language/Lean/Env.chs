{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
module Language.Lean.Env
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
  ) where

import Control.Exception (bracket)
import Control.Lens
import Data.IORef
import Foreign
import Foreign.C
import System.IO.Unsafe

{#import Language.Lean.Internal.Decl#}
{#import Language.Lean.Internal.Env#}
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
envForget x = tryGetLeanValue $ lean_env_forget x

-- |  Return the declaration with the given name in the environment if any.
{#fun unsafe lean_env_forget
     { `Env', `OutEnvPtr', `OutExceptionPtr' } -> `Bool' #}

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
envFoldUnivs = runLeanFold wrapNameVisitFn mkLeanValue lean_env_for_each_univ

foreign import ccall "wrapper" wrapNameVisitFn :: WrapLeanVisitFn NamePtr

{#fun lean_env_for_each_univ
     { `Env'
     , id `FunPtr (NamePtr -> IO ())'
     , `OutExceptionPtr'
     } -> `Bool' #}
