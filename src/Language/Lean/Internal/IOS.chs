{-|
Module      : Language.Lean.Internal.IOS
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Internal declarations for IOState.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_HADDOCK not-home #-}
module Language.Lean.Internal.IOS
  ( IOState
  , withIOState
  , someIOS
  , type IOStateType(..)
  , SomeIOState
  , SomeIOStatePtr
  , OutSomeIOStatePtr
  , withSomeIOState
  , BufferedIOState
  , withBufferedIOState
  ) where

import Foreign
import System.IO (stderr, stdout)


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

_unused :: a
_unused = undefined stderr stdout

-- | Internal state used for bindings
newtype SomeIOState = SomeIOState (ForeignPtr SomeIOState)

foreign import ccall unsafe "&lean_ios_del"
  lean_ios_del_ptr :: FunPtr (Ptr SomeIOState -> IO ())


-- | This describes the type of the @IOState@.
data IOStateType
   = Standard -- ^ A standard 'IOState'
   | Buffered -- ^ A buffered 'IOState'

-- | The IO State object
--
-- Lean uses two channels for sending output to the user:
--
--  * A /regular/ output channel, which consists of messages normally
--    printed to 'stdout'.
--  * A /diagnostic/ output channel, which consists of debugging
--    messages that are normally printed to 'stderr'.
--
-- This module currently provides two different 'IOState' types:
--
--  * A /standard/ IO state that sends regular output to 'stdout' and
--    diagnostic output to 'stderr'.
--  * A /buffered/ IO state type that stores output internally, and
--    provides methods for getting output as strings.
--
-- To prevent users from accidentally using the wrong type of output,
-- the 'IOState' has an extra type-level parameter used to
-- indicate the type of channel.  Most Lean operations support both
-- types of channels and either can be used.  Operations specific
-- to a particular channel can use this type parameter to ensure
-- users do not call the function on the wrong type of channel.  In
-- addition, we provide a function @stateTypeRepr@ to allow users
-- to determine the type of channel.
newtype IOState (tp :: IOStateType) = IOState (ForeignPtr SomeIOState)

-- | Lift an arbitray IOState to SomeIOState
someIOS :: IOState tp -> SomeIOState
someIOS (IOState p) = SomeIOState (castForeignPtr p)

instance IsLeanValue (IOState tp) (Ptr SomeIOState) where
  mkLeanValue = fmap IOState . newForeignPtr lean_ios_del_ptr

-- | Run a computation with an io state.
withIOState :: IOState tp -> (Ptr SomeIOState -> IO a) -> IO a
withIOState (IOState ptr) f = seq ptr $ withForeignPtr ptr (f . castPtr)

-- | Type synonym for @c2hs@ to use specifically for functions that
-- expected buffered IO state.
type BufferedIOState = IOState 'Buffered

-- | Function @c2hs@ uses to pass 'BufferedIOState' values to Lean
withBufferedIOState :: IOState 'Buffered -> (Ptr SomeIOState -> IO a) -> IO a
withBufferedIOState = withIOState

{#pointer lean_ios as BufferedIOState foreign newtype nocode#}

-- | Function @c2hs@ uses to pass 'SomeIOState' values to Lean
withSomeIOState :: SomeIOState -> (Ptr SomeIOState -> IO a) -> IO a
withSomeIOState (SomeIOState p) f = seq p $ withForeignPtr p (f . castPtr)

{#pointer lean_ios as SomeIOState foreign newtype nocode#}

-- | Haskell type for @lean_ios@ FFI parameters.
type SomeIOStatePtr = Ptr SomeIOState

-- | Haskell type for @lean_ios*@ FFI parameters.
{#pointer *lean_ios as OutSomeIOStatePtr -> SomeIOStatePtr #}
