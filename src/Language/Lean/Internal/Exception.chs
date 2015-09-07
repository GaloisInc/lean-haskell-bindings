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
  , tryGetLeanMaybeValue
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

{#fun unsafe lean_exception_get_message
  { `ExceptionPtr' } -> `String' getLeanString* #}

{#enum lean_exception_kind as ExceptionKind { upcaseFirstLetter }
         deriving (Eq)#}

{#fun unsafe lean_exception_get_kind
  { `ExceptionPtr' } -> `ExceptionKind' #}

-- | Convert the ExceptionPtr to a Lean exception and throw
-- it while releasing the ExceptionPtr
throwLeanException :: ExceptionPtr -> IO a
throwLeanException ptr = do
  (throwIO =<< leanExceptionFromPtr ptr)
    `finally` lean_exception_del ptr

leanExceptionFromPtr :: ExceptionPtr -> IO LeanException
leanExceptionFromPtr ptr = do
  k <- lean_exception_get_kind ptr
  msg <- lean_exception_get_message ptr
  return $! LeanException (getLeanExceptionKind k) msg

-- | Create a Lean kernel exception from the given messasge.
leanKernelException :: String -> LeanException
leanKernelException = LeanException LeanKernelException

-- | Create a Lean other exception from the given messasge.
leanOtherException :: String -> LeanException
leanOtherException = LeanException LeanOtherException

getLeanExceptionKind :: ExceptionKind -> LeanExceptionKind
getLeanExceptionKind k = do
  case k of
    LEAN_NULL_EXCEPTION   -> error "getLeanException not given an exception"
    LEAN_SYSTEM_EXCEPTION -> LeanSystemException
    LEAN_OUT_OF_MEMORY    -> LeanOutOfMemory
    LEAN_INTERRUPTED      -> LeanOutOfMemory
    LEAN_KERNEL_EXCEPTION -> LeanKernelException
    LEAN_OTHER_EXCEPTION  -> LeanOtherException

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

-- | Run a lean partial function
runLeanPartialFn :: Storable a
                 => LeanPartialFn a
                 -> IO a
runLeanPartialFn alloc_fn =
  alloca $ \ret_ptr -> do
    runLeanPartialAction (alloc_fn ret_ptr)
    peek ret_ptr
{-# INLINE runLeanPartialFn #-}

-- | Run a lean partial function where false does not automatically imply
-- an exception was thrown.
runLeanMaybeFn :: Storable p
               => LeanPartialFn p
               -> IO (Maybe p)
runLeanMaybeFn alloc_fn =
  alloca $ \ret_ptr -> do
    alloca $ \ex_ptr -> do
      poke ex_ptr nullPtr
      success <- alloc_fn ret_ptr ex_ptr
      if success then do
        r <- peek ret_ptr
        return $! Just r
      else do
        ptr <- peek ex_ptr
        if ptr == nullPtr then
          return $! Nothing
        else
          throwLeanException ptr
{-# INLINE runLeanMaybeFn #-}

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
tryAllocLeanValue = \alloc_fn -> mkLeanValue =<< runLeanPartialFn alloc_fn
{-# INLINE tryAllocLeanValue #-}

-- | Try to run a Lean partial function that returns a Lean value
-- that will need to be freed.
--
-- Other than allocating a new value or exception, the function
-- should be be pure.
tryGetLeanValue :: IsLeanValue a p
                => LeanPartialFn p
                -> a
tryGetLeanValue alloc_fn = unsafePerformIO $ do
  mkLeanValue =<< runLeanPartialFn alloc_fn
{-# INLINE tryGetLeanValue #-}

-- | Try to run a Lean function that may return a Lean value
-- that will need to be freed.
--
-- Other than allocating a new value or throwing an exception,
-- the function should be pure.
tryGetLeanMaybeValue :: IsLeanValue a p
                     => LeanPartialFn p
                     -> Maybe a
tryGetLeanMaybeValue alloc_fn = unsafePerformIO $ do
  traverse mkLeanValue =<< runLeanMaybeFn alloc_fn

{-# INLINE tryGetLeanMaybeValue #-}

-- | Try to run a Lean partial function that returns an enum type
tryGetEnum :: (Enum a) => LeanPartialFn CInt -> a
tryGetEnum alloc_fn =
  toEnum $ fromIntegral $ unsafePerformIO $ runLeanPartialFn alloc_fn
