{-|
Module      : Language.Lean.Module
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Declarations for importing and exporting modules and accessing Lean paths
-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Trustworthy #-}
module Language.Lean.Module
  ( envImport
  , envExport
  , stdPath
  , hottPath
  ) where

import Foreign
import Foreign.C

import Language.Lean.List

{#import Language.Lean.Internal.Decl#}
{#import Language.Lean.Internal.Exception#}
import Language.Lean.Internal.Exception.Unsafe
{#import Language.Lean.Internal.Name#}
{#import Language.Lean.Internal.IOS#}
{#import Language.Lean.Internal.String#}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"
#include "lean_expr.h"
#include "lean_decl.h"
#include "lean_ios.h"
#include "lean_module.h"

-- | Import the given module names into the lean environment
envImport :: IOState tp -> Env -> List Name -> IO Env
envImport s e names = tryAllocLeanValue $ lean_env_import e (someIOS s) names

{#fun lean_env_import
 { `Env'
 , `SomeIOState'
 , `ListName'
 , `OutEnvPtr'
 , `OutExceptionPtr'
 } -> `Bool' #}

-- | Export the lean environment to a path.
envExport :: Env -> FilePath -> IO ()
envExport e path = runLeanPartialAction $ lean_env_export e path

{#fun lean_env_export
   { `Env'
   , withLeanStringPtr* `String'
   , `OutExceptionPtr'
   } -> `Bool' #}

-- | Path to lean standard library (extracted from LEAN_PATH)
stdPath :: String
stdPath = tryGetLeanValue $ lean_get_std_path

{#fun unsafe lean_get_std_path
 { id `Ptr CString', `OutExceptionPtr' } -> `Bool' #}

-- | Path to lean hott library (extrcted from HLEAN_PATH)
hottPath :: String
hottPath = tryGetLeanValue $ lean_get_hott_path

{#fun unsafe lean_get_hott_path
 { id `Ptr CString', `OutExceptionPtr' } -> `Bool' #}
