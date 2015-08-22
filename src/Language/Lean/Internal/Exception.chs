{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Lean.Internal.Exception
  ( LeanException(..)
  , LeanExceptionKind(..)
    -- * FFI types
  , ExceptionPtr
  , OutExceptionPtr
  , throwLeanException
  , leanKernelException
  , leanOtherException
    -- * Partial functions
  , LeanPartialFn
  , tryAllocString
  , tryGetBool
  , tryGetDouble
  , tryGetInt
  , tryGetUInt
  , tryAllocLeanValue
  ) where

import Control.Exception (finally, throwIO)
import Control.Monad
import Data.Coerce (coerce)
import Foreign
import Foreign.C
import System.IO.Unsafe

import Language.Lean.Exception
import Language.Lean.Internal.String

#include "lean_macros.h"
#include "lean_exception.h"

{#pointer lean_exception as ExceptionPtr newtype#}
{#pointer *lean_exception as OutExceptionPtr -> ExceptionPtr #}

deriving instance Storable ExceptionPtr

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

leanKernelException :: String -> LeanException
leanKernelException = LeanException LeanKernelException

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

type LeanPartialFn a = (Ptr a -> Ptr ExceptionPtr -> IO Bool)

tryLeanPartialFn :: Storable a
                 => (a -> IO r)
                 -> LeanPartialFn a
                 -> r
tryLeanPartialFn next alloc_fn = unsafePerformIO $ do
  alloca $ \ret_ptr ->do
    alloca $ \ex_ptr ->do
      success <- alloc_fn ret_ptr ex_ptr
      when (not success) $ do
        ex <- peek ex_ptr
        throwLeanException ex
      next =<< peek ret_ptr

tryGetBool :: LeanPartialFn CInt -> Bool
tryGetBool = tryLeanPartialFn (return . toEnum . fromIntegral)

tryGetUInt :: LeanPartialFn CUInt -> Word32
tryGetUInt = tryLeanPartialFn $ return . coerce

tryGetInt :: LeanPartialFn CInt -> Int32
tryGetInt = tryLeanPartialFn $ return . coerce

tryGetDouble :: LeanPartialFn CDouble -> Double
tryGetDouble = tryLeanPartialFn $ return . coerce

tryAllocString :: LeanPartialFn CString -> String
tryAllocString = tryLeanPartialFn getLeanString

tryAllocLeanValue :: FunPtr (Ptr a -> IO ())
                   -> LeanPartialFn (Ptr a)
                   -> ForeignPtr a
tryAllocLeanValue free_fn = tryLeanPartialFn $ newForeignPtr free_fn
