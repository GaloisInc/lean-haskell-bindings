{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Lean.Options
  ( Options
  , OptionsPtr
  , withOptionsPtr
  ) where

import Control.Exception (assert)
import Foreign
import Foreign.C
import System.IO.Unsafe


{#import Language.Lean.Internal.Exception #}
{#import Language.Lean.Internal.String #}
{#import Language.Lean.Name #}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"

{#pointer lean_options as OptionsPtr -> Options#}

{#fun unsafe lean_options_mk_empty
  { id `Ptr OptionsPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

newtype Options = Options (ForeignPtr Options)

-- | Run an action with the underlying pointer.
withOptionsPtr :: Options -> (OptionsPtr -> IO a) -> IO a
withOptionsPtr (Options x) = withForeignPtr x
