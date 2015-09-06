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

{#import Language.Lean.Internal.Exception#}

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

foreign import ccall unsafe "&lean_ios_del"
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

-- | Type synonym for @c2hs@ to use specifically for functions that
-- expected buffered IO state.
type BufferedIOState = IOState 'Buffered

-- | Function @c2hs@ uses to pass @BufferedIOState@ values to Lean
withBufferedIOState :: IOState 'Buffered -> (Ptr SomeIOState -> IO a) -> IO a
withBufferedIOState = withIOState

{#pointer lean_ios as BufferedIOState foreign newtype nocode#}

-- | Function @c2hs@ uses to pass @SomeIOState@ values to Lean
withSomeIOState :: SomeIOState -> (Ptr SomeIOState -> IO a) -> IO a
withSomeIOState (SomeIOState p) f = withForeignPtr p (f . castPtr)

{#pointer lean_ios as SomeIOState foreign newtype nocode#}

-- | Haskell type for @lean_ios@ FFI parameters.
type SomeIOStatePtr = Ptr SomeIOState

-- | Haskell type for @lean_ios*@ FFI parameters.
{#pointer *lean_ios as OutSomeIOStatePtr -> SomeIOStatePtr #}
