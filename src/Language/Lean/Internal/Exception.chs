{-# LANGUAGE DoAndIfThenElse #-}
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

import Control.Exception
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

-- | A lean partial function ia
type LeanPartialFn a = (Ptr a -> Ptr ExceptionPtr -> IO Bool)

runLeanPartialFn :: Storable a
                 => LeanPartialFn a
                 -> IO a
runLeanPartialFn alloc_fn =
  alloca $ \ret_ptr ->
    alloca $ \ex_ptr -> do
      success <- alloc_fn ret_ptr ex_ptr
      if success then
        peek ret_ptr
      else
        throwLeanException =<< peek ex_ptr

{-# INLINE runLeanPartialFn #-}

tryLeanPartialFn :: Storable a
                 => (a -> IO r)
                 -> LeanPartialFn a
                 -> r
tryLeanPartialFn next alloc_fn = unsafePerformIO $ do
  runLeanPartialFn alloc_fn >>= next
{-# INLINE tryLeanPartialFn #-}

-- | Try to run a Lean partial function that returns a Boolean argument.
tryGetBool :: LeanPartialFn CInt -> Bool
tryGetBool = tryLeanPartialFn (return . toEnum . fromIntegral)

-- | Try to run a Lean partial function that returns a unsigned integer.
tryGetUInt :: LeanPartialFn CUInt -> Word32
tryGetUInt = tryLeanPartialFn $ return . coerce

-- | Try to run a Lean partial function that returns a signed integer.
tryGetInt :: LeanPartialFn CInt -> Int32
tryGetInt = tryLeanPartialFn $ return . coerce

-- | Try to run a Lean partial function that returns a double.
tryGetDouble :: LeanPartialFn CDouble -> Double
tryGetDouble = tryLeanPartialFn $ return . coerce

-- | Try to run a Lean partial function that returns a string.
tryAllocString :: LeanPartialFn CString -> String
tryAllocString = tryLeanPartialFn getLeanString

-- | Try to run a Lean partial function that returns a Lean value
-- that will need to be freed.
tryAllocLeanValue :: FunPtr (Ptr a -> IO ())
                     -- ^ Pointer to function that releases resource.
                  -> LeanPartialFn (Ptr a)
                  -> ForeignPtr a
tryAllocLeanValue free_fn = tryLeanPartialFn $ newForeignPtr free_fn
