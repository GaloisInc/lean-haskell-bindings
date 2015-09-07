{-|
Module      : Language.Lean.IOS
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Operations for creating and manipulating an 'IOState', an object
for controlling how Lean sends console output to the user.

-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Lean.IOS
  ( IOState
  , type IOStateType(..)
    -- * Standard IOState
  , mkStandardIOState
  , mkStandardIOStateWithOptions
    -- * Buffered IOState
  , mkBufferedIOState
  , mkBufferedIOStateWithOptions
  , getRegularOutput
  , getDiagnosticOutput
  , resetRegularOutput
  , resetDiagnosticOutput
    -- * Operations on IO State
  , IOStateTypeRepr(..)
  , stateTypeRepr
  , getStateOptions
  , setStateOptions
    -- * Operations using IOState
  , ppExpr
  ) where

import Foreign
import Foreign.C
import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)

{#import Language.Lean.Internal.Decl#}
{#import Language.Lean.Internal.Exception#}
{#import Language.Lean.Internal.Expr#}
{#import Language.Lean.Internal.IOS#}
{#import Language.Lean.Internal.Options#}

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
-- Standard IOState

-- | Create a standard IO state object with default options.
mkStandardIOState :: IO (IOState 'Standard)
mkStandardIOState = mkStandardIOStateWithOptions emptyOptions

-- | Create a standard IO state object with the given options.
mkStandardIOStateWithOptions :: Options -> IO (IOState 'Standard)
mkStandardIOStateWithOptions o = tryAllocLeanValue $ lean_ios_mk_std o

{#fun unsafe lean_ios_mk_std
 { `Options', `OutSomeIOStatePtr', `OutExceptionPtr' } -> `Bool' #}

------------------------------------------------------------------------
-- Buffered IOState

-- | Create IO state object that sends the regular and diagnostic output to
-- string buffers with the given options.
mkBufferedIOState :: IO (IOState 'Buffered)
mkBufferedIOState = mkBufferedIOStateWithOptions emptyOptions

-- | Create IO state object that sends the regular and diagnostic output to
-- string buffers with the given options.
mkBufferedIOStateWithOptions :: Options -> IO (IOState 'Buffered)
mkBufferedIOStateWithOptions o = tryAllocLeanValue $ lean_ios_mk_buffered o

{#fun unsafe lean_ios_mk_buffered
 { `Options', `OutSomeIOStatePtr', `OutExceptionPtr' } -> `Bool' #}

-- | Return the regular output associated with a state.
getRegularOutput :: IOState 'Buffered -> IO String
getRegularOutput s = tryAllocLeanValue $ lean_ios_get_regular s

{#fun unsafe lean_ios_get_regular
 { `BufferedIOState', id `Ptr CString', `OutExceptionPtr' } -> `Bool' #}

-- | Reset the regular output associated with a state.
resetRegularOutput :: IOState 'Buffered -> IO ()
resetRegularOutput s = runLeanPartialAction $ lean_ios_reset_regular s

{#fun unsafe lean_ios_reset_regular
 { `BufferedIOState',`OutExceptionPtr' } -> `Bool' #}

-- | Return the diagnostic output associated with a state.
getDiagnosticOutput :: IOState 'Buffered -> IO String
getDiagnosticOutput s = tryAllocLeanValue $ lean_ios_get_diagnostic s

{#fun unsafe lean_ios_get_diagnostic
 { `BufferedIOState', id `Ptr CString', `OutExceptionPtr' } -> `Bool' #}

-- | Clear the diagnostic output associated with a state.
resetDiagnosticOutput :: IOState 'Buffered -> IO ()
resetDiagnosticOutput s = runLeanPartialAction $ lean_ios_reset_diagnostic s

{#fun unsafe lean_ios_reset_diagnostic
 { `BufferedIOState',`OutExceptionPtr' } -> `Bool' #}

------------------------------------------------------------------------
-- IOState introspection

-- | Flag indicating the type of state.
--
-- This is implemented as a GADT to allow client code to specialize an
-- @IOState@ to the appropriate subtype.
data IOStateTypeRepr (tp :: IOStateType) where
  StandardRepr :: IOStateTypeRepr 'Standard
  BufferedRepr :: IOStateTypeRepr 'Buffered

deriving instance Show (IOStateTypeRepr tp)

-- | Get the type of the channel
stateTypeRepr :: IOState tp -> IOStateTypeRepr tp
stateTypeRepr s
  | lean_ios_is_std (someIOS s) = unsafeCoerce StandardRepr
  | otherwise                   = unsafeCoerce BufferedRepr

-- Return true if this is a IO state
{#fun pure unsafe lean_ios_is_std { `SomeIOState' } -> `Bool' #}

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

------------------------------------------------------------------------
-- Pretty print expression

-- | Pretty print an expression
ppExpr :: Env -> IOState tp -> Expr -> IO String
ppExpr env s e = tryAllocLeanValue $ lean_expr_to_pp_string env (someIOS s) e

{#fun unsafe lean_expr_to_pp_string
  { `Env'
  , `SomeIOState'
  , `Expr'
  , id `Ptr CString'
  , `OutExceptionPtr'
  } -> `Bool' #}