{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Lean.Internal.IOS
  ( IOState
  , type IOStateType(..)
    -- * Stadnard IOState
  , mkStandardIOState
    -- * Buffered IOState
  , mkBufferedIOState
  , getRegularOutput
  , getDiagnosticOutput
  , resetRegularOutput
  , resetDiagnosticOutput
    -- * Operations on IO State
  , IOStateTypeRepr(..)
  , iosTypeRepr
  , iosGetOptions
  , iosSetOptions
    -- * Operations using IOState
  , ppExpr
    -- * External FFI declarations
  , SomeIOState
  , withSomeIOState
  , someIOS
  ) where

import Foreign
import Foreign.C
import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)

{#import Language.Lean.Internal.Exception#}
{#import Language.Lean.Internal.Options#}
{#import Language.Lean.Internal.Expr#}
{#import Language.Lean.Internal.Env#}

-- | This describes the type of the @IOState@.
data IOStateType
   = Standard
   | Buffered

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

-- | Internal state used for bindings
newtype SomeIOState = SomeIOState (ForeignPtr SomeIOState)

foreign import ccall "&lean_ios_del"
  lean_ios_del_ptr :: FunPtr (Ptr SomeIOState -> IO ())

-- | The IO State
newtype IOState (tp :: IOStateType) = IOState (ForeignPtr SomeIOState)

-- | Lift an arbitray IOState to SomeIOState
someIOS :: IOState tp -> SomeIOState
someIOS (IOState p) = SomeIOState (castForeignPtr p)

instance IsLeanValue (IOState tp) (Ptr SomeIOState) where
  mkLeanValue = fmap IOState . newForeignPtr lean_ios_del_ptr

-- | Run a computation with an io state.
withIOState :: IOState tp -> (Ptr SomeIOState -> IO a) -> IO a
withIOState (IOState ptr) f = withForeignPtr ptr (f . castPtr)

type BufferedIOState = IOState 'Buffered

withBufferedIOState :: IOState 'Buffered -> (Ptr SomeIOState -> IO a) -> IO a
withBufferedIOState = withIOState

{#pointer lean_ios as BufferedIOState foreign newtype nocode#}

withSomeIOState :: SomeIOState -> (Ptr SomeIOState -> IO a) -> IO a
withSomeIOState (SomeIOState p) f = withForeignPtr p (f . castPtr)

{#pointer lean_ios as SomeIOState foreign newtype nocode#}

type SomeIOStatePtr = Ptr SomeIOState
{#pointer *lean_ios as SomeIOStatePtrPtr -> SomeIOStatePtr #}

------------------------------------------------------------------------
-- Standard IOState

-- | Create IO state object that sends the regular and diagnostic output to
-- standard out and standard error
mkStandardIOState :: Options -> IO (IOState 'Standard)
mkStandardIOState o = tryAllocLeanValue $ lean_ios_mk_std o

{#fun unsafe lean_ios_mk_std
 { `Options', `SomeIOStatePtrPtr', `OutExceptionPtr' } -> `Bool' #}

------------------------------------------------------------------------
-- Buffered IOState

-- | Create IO state object that sends the regular and diagnostic output to
-- string buffers.
mkBufferedIOState :: Options -> IO (IOState 'Buffered)
mkBufferedIOState o = tryAllocLeanValue $ lean_ios_mk_buffered o

{#fun unsafe lean_ios_mk_buffered
 { `Options', `SomeIOStatePtrPtr', `OutExceptionPtr' } -> `Bool' #}

getRegularOutput :: IOState 'Buffered -> IO String
getRegularOutput s = tryAllocString $ lean_ios_get_regular s

{#fun unsafe lean_ios_get_regular
 { `BufferedIOState', id `Ptr CString', `OutExceptionPtr' } -> `Bool' #}

getDiagnosticOutput :: IOState 'Buffered -> IO String
getDiagnosticOutput s = tryAllocString $ lean_ios_get_diagnostic s

{#fun unsafe lean_ios_get_diagnostic
 { `BufferedIOState', id `Ptr CString', `OutExceptionPtr' } -> `Bool' #}

resetRegularOutput :: IOState 'Buffered -> IO ()
resetRegularOutput s = runLeanPartialAction $ lean_ios_reset_regular s

{#fun unsafe lean_ios_reset_regular
  { `BufferedIOState',`OutExceptionPtr' } -> `Bool' #}

resetDiagnosticOutput :: IOState 'Buffered -> IO ()
resetDiagnosticOutput s = runLeanPartialAction $ lean_ios_reset_diagnostic s

{#fun unsafe lean_ios_reset_diagnostic
  { `BufferedIOState',`OutExceptionPtr' } -> `Bool' #}

------------------------------------------------------------------------
-- IOState introspection

data IOStateTypeRepr (tp :: IOStateType) where
  StandardRepr :: IOStateTypeRepr 'Standard
  BufferedRepr :: IOStateTypeRepr 'Buffered

deriving instance Show (IOStateTypeRepr tp)

-- | Return the representation of the type.
iosTypeRepr :: IOState tp -> IOStateTypeRepr tp
iosTypeRepr s
  | lean_ios_is_std (someIOS s) = unsafeCoerce StandardRepr
  | otherwise                   = unsafeCoerce BufferedRepr

-- Return true if this is a IO state
{#fun pure unsafe lean_ios_is_std { `SomeIOState' } -> `Bool' #}

------------------------------------------------------------------------
-- IOState options

iosGetOptions :: IOState tp -> IO Options
iosGetOptions ios = tryAllocLeanValue $ lean_ios_get_options (someIOS ios)

{#fun unsafe lean_ios_get_options
 { `SomeIOState', `OutOptionsPtr', `OutExceptionPtr' } -> `Bool' #}


iosSetOptions :: IOState tp -> Options -> IO ()
iosSetOptions ios ops = runLeanPartialAction $ lean_ios_set_options (someIOS ios) ops

{#fun unsafe lean_ios_set_options
 { `SomeIOState', `Options', `OutExceptionPtr' } -> `Bool' #}


------------------------------------------------------------------------
-- Pretty print expression

ppExpr :: Env -> IOState tp -> Expr -> IO String
ppExpr env s e = tryAllocString $ lean_expr_to_pp_string env (someIOS s) e

{#fun unsafe lean_expr_to_pp_string
  { `Env'
  , `SomeIOState'
  , `Expr'
  , id `Ptr CString'
  , `OutExceptionPtr'
  } -> `Bool' #}
