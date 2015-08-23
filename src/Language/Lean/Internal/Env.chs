{-# LANGUAGE ForeignFunctionInterface #-}
module Language.Lean.Internal.Env
  ( Env
    -- * Foreign interface
  , EnvPtr
  , OutEnvPtr
  , tryAllocEnv
  , withEnv
  ) where

import Foreign

{#import Language.Lean.Internal.Decl#}
{#import Language.Lean.Internal.Exception#}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"
#include "lean_expr.h"
#include "lean_decl.h"
#include "lean_env.h"

-- | Call a C layer function that attempts to allocate a
-- new declaration.
tryAllocEnv :: LeanPartialFn EnvPtr -> Env
tryAllocEnv mk =
  Env $ tryAllocLeanValue lean_env_del_ptr $ mk

foreign import ccall "&lean_env_del"
  lean_env_del_ptr :: FunPtr (EnvPtr -> IO ())
