{-# LANGUAGE ForeignFunctionInterface #-}
module Language.Lean.Module
  ( envImport
  , envExport
  , stdPath
  , hottPath
  ) where

import Foreign
import Foreign.C

import Language.Lean.List

{#import Language.Lean.Internal.Env#}
{#import Language.Lean.Internal.Exception#}
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

-- Import the given modules (i.e., pre-compiled .olean files.
envImport :: Env -> IOState tp -> List Name -> IO Env
envImport e s names = tryAllocLeanValue $ lean_env_import e (someIOS s) names

{#fun lean_env_import
   { `Env'
   , `SomeIOState'
   , `ListName'
   , `OutEnvPtr'
   , `OutExceptionPtr'
   } -> `Bool' #}

-- Import the given modules (i.e., pre-compiled .olean files.
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
