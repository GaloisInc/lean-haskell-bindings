{-|
Module      : Language.Lean.Internal.IOS
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

This provides methods for getting and setting IOS options.  The actual
'IOState' declarations appear in the 'Language.Lean.Internal.Exception'.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}
module Language.Lean.Internal.IOS
    -- * IOState Options
  ( getStateOptions
  , setStateOptions
  ) where

import Foreign
import Foreign.C (CInt(..))

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
#include "lean_ios.h"

------------------------------------------------------------------------
-- IOState options

-- | Get the options associated with the state.
getStateOptions :: IOState tp -> IO Options
getStateOptions ios = tryAllocLeanValue $ lean_ios_get_options (someIOS ios)

{#fun unsafe lean_ios_get_options
 { `SomeIOState', `OutOptionsPtr', `OutExceptionPtr' } -> `Bool' #}

-- | Set the options associated with the state.
setStateOptions :: IOState tp -> Options -> IO ()
setStateOptions ios ops = runLeanPartialAction $ lean_ios_set_options (someIOS ios) ops

{#fun unsafe lean_ios_set_options
 { `SomeIOState', `Options', `OutExceptionPtr' } -> `Bool' #}
