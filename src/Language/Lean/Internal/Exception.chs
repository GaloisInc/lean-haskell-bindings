{-|
Module      : Language.Lean.Internal.Exception
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Internal operations for working with Lean exceptions.
-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE Trustworthy #-}
module Language.Lean.Internal.Exception
  ( LeanException(..)
  , LeanExceptionKind(..)
    -- * FFI types
  , ExceptionPtr
  , OutExceptionPtr
  , throwLeanException
  , leanKernelException
  , leanOtherException
    -- * Partial operations
  , LeanPartialAction
  , runLeanPartialAction
  , LeanPartialFn
  , IsLeanValue(..)
    -- * Functions that return a result in IO
  , tryAllocLeanValue
    -- * Functions that are pure.
  , tryGetEnum
  , tryGetLeanValue
  ) where

import Control.Exception
import Control.Monad (when)
import Data.Coerce (coerce)
import Data.Typeable
import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import Language.Lean.Internal.String

#include "lean_macros.h"
#include "lean_exception.h"

-- | Information about the Kind of exception thrown.
data LeanExceptionKind
   = LeanSystemException
   | LeanOutOfMemory
   | LeanInterrupted
   | LeanKernelException
     -- ^ Errors from Lean misuse
   | LeanOtherException
  deriving (Show)

-- | An exception thrown by Lean
data LeanException
   = LeanException { leanExceptionKind :: !LeanExceptionKind
                   , leanExceptionName :: !String
                   }
 deriving (Typeable, Show)

instance Exception LeanException

------------------------------------------------------------------------
-- FFI Declarations

-- | Pointer used as input parameter for exceptions in FFI bindings
{#pointer lean_exception as ExceptionPtr -> LeanException#}
-- | Pointer used as output parameter for exceptions in FFI bindings
{#pointer *lean_exception as OutExceptionPtr -> ExceptionPtr #}

{#fun unsafe lean_exception_del
  { `ExceptionPtr' } -> `()' #}

{#fun pure unsafe lean_exception_get_message
  { `ExceptionPtr' } -> `String' getLeanString* #}

{#enum lean_exception_kind as ExceptionKind { upcaseFirstLetter }
         deriving (Eq)#}

{#fun pure unsafe lean_exception_get_kind
  { `ExceptionPtr' } -> `ExceptionKind' #}

-- | Convert the ExceptionPtr to a Lean exception and throw
-- it while releasing the ExceptionPtr
throwLeanException :: ExceptionPtr -> IO a
throwLeanException ptr = do
  (throwIO $! leanExceptionFromPtr ptr)
    `finally` lean_exception_del ptr

leanExceptionFromPtr :: ExceptionPtr -> LeanException
leanExceptionFromPtr ptr = do
  LeanException (getLeanExceptionKind ptr)
                (lean_exception_get_message ptr)

-- | Create a Lean kernel exception from the given messasge.
leanKernelException :: String -> LeanException
leanKernelException = LeanException LeanKernelException

-- | Create a Lean other exception from the given messasge.
leanOtherException :: String -> LeanException
leanOtherException = LeanException LeanOtherException

getLeanExceptionKind :: ExceptionPtr -> LeanExceptionKind
getLeanExceptionKind ptr = do
  case lean_exception_get_kind ptr of
    LEAN_NULL_EXCEPTION -> do
      error "getLeanException not given an exception"
    LEAN_SYSTEM_EXCEPTION -> do
      LeanSystemException
    LEAN_OUT_OF_MEMORY -> do
      LeanOutOfMemory
    LEAN_INTERRUPTED -> do
      LeanOutOfMemory
    LEAN_KERNEL_EXCEPTION -> do
      LeanKernelException
    LEAN_OTHER_EXCEPTION -> do
      LeanOtherException

------------------------------------------------------------------------
-- Partial functions

-- | A lean partial function is an action that may fail
type LeanPartialAction = (Ptr ExceptionPtr -> IO Bool)

-- | Run a lean partial action, throwing an exception if it fails.
runLeanPartialAction :: LeanPartialAction
                     -> IO ()
runLeanPartialAction action =
  alloca $ \ex_ptr -> do
    success <- action ex_ptr
    when (not success) $ do
      throwLeanException =<< peek ex_ptr
{-# INLINE runLeanPartialAction #-}

-- | A lean partial function is a function that returns a value of type @a@, but
-- may fail.
type LeanPartialFn a = (Ptr a -> LeanPartialAction)

runLeanPartialFn :: Storable a
                 => LeanPartialFn a
                 -> IO a
runLeanPartialFn alloc_fn =
  alloca $ \ret_ptr -> do
    runLeanPartialAction (alloc_fn ret_ptr)
    peek ret_ptr
{-# INLINE runLeanPartialFn #-}

tryLeanPartialFn :: Storable a
                     => (a -> IO r)
                     -> LeanPartialFn a
                     -> IO r
tryLeanPartialFn = \next alloc_fn -> runLeanPartialFn alloc_fn >>= next
{-# INLINE tryLeanPartialFn #-}

tryPureLeanPartialFn :: Storable a
                 => (a -> IO r)
                 -> LeanPartialFn a
                 -> r
tryPureLeanPartialFn = \next alloc_fn -> unsafePerformIO $ do
  runLeanPartialFn alloc_fn >>= next
{-# INLINE tryPureLeanPartialFn #-}

-- | Typeclass that associates Haskell types with their type in
-- the FFI layer.
class Storable p => IsLeanValue v p | v -> p where
  -- | Create a Haskell value from a FFI value.
  mkLeanValue :: p -> IO v

instance IsLeanValue Bool CInt where
  mkLeanValue = return . toEnum . fromIntegral

instance IsLeanValue Word32 CUInt where
  mkLeanValue = return . coerce

instance IsLeanValue Int32 CInt where
  mkLeanValue = return . coerce

instance IsLeanValue Double CDouble where
  mkLeanValue = return . coerce

instance IsLeanValue String CString where
  mkLeanValue = getLeanString

-- | Try to run a Lean partial function that returns a Lean value
-- that will need to be freed.
tryAllocLeanValue :: IsLeanValue a p
                  => LeanPartialFn p
                  -> IO a
tryAllocLeanValue = tryLeanPartialFn mkLeanValue
{-# INLINE tryAllocLeanValue #-}

-- | Try to run a Lean partial function that returns a Lean value
-- that will need to be freed.
--
-- Other than allocating a new value or exception, the function
-- must be pure
tryGetLeanValue :: IsLeanValue a p
                => LeanPartialFn p
                -> a
tryGetLeanValue = tryPureLeanPartialFn mkLeanValue
{-# INLINE tryGetLeanValue #-}

-- | Try to run a Lean partial function that returns an enum type
tryGetEnum :: (Enum a) => LeanPartialFn CInt -> a
tryGetEnum = tryPureLeanPartialFn $ return . toEnum . fromIntegral
