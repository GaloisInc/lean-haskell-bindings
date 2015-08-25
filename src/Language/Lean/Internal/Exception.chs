{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    -- * Partial operations
  , LeanPartialAction
  , runLeanPartialAction
  , LeanPartialFn
  , IsLeanValue(..)
    -- * Functions that return a result in IO
  , tryAllocString
  , tryAllocLeanValue
    -- * Functions that are pure.
  , tryGetString
  , tryGetBool
  , tryGetDouble
  , tryGetEnum
  , tryGetInt
  , tryGetUInt
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

-- | A lean partial function is an action that may fail
type LeanPartialAction = (Ptr ExceptionPtr -> IO Bool)

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
type LeanPartialFn a = (Ptr a -> Ptr ExceptionPtr -> IO Bool)

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
tryPureLeanPartialFn next = unsafePerformIO . tryLeanPartialFn  next
{-# INLINE tryPureLeanPartialFn #-}

-- | Try to run a Lean partial function that returns a Boolean argument.
tryGetBool :: LeanPartialFn CInt -> Bool
tryGetBool = tryPureLeanPartialFn (return . toEnum . fromIntegral)

-- | Try to run a Lean partial function that returns a unsigned integer.
tryGetUInt :: LeanPartialFn CUInt -> Word32
tryGetUInt = tryPureLeanPartialFn $ return . coerce

-- | Try to run a Lean partial function that returns a signed integer.
tryGetInt :: LeanPartialFn CInt -> Int32
tryGetInt = tryPureLeanPartialFn $ return . coerce

-- | Try to run a Lean partial function that returns a double.
tryGetDouble :: LeanPartialFn CDouble -> Double
tryGetDouble = tryPureLeanPartialFn $ return . coerce

-- | Try to run a Lean partial function that returns an enum type
tryGetEnum :: (Enum a) => LeanPartialFn CInt -> a
tryGetEnum = tryPureLeanPartialFn $ return . toEnum . fromIntegral

-- | Try to run a Lean partial function that returns a string.
tryAllocString :: LeanPartialFn CString -> IO String
tryAllocString = tryLeanPartialFn getLeanString

-- | Try to run a Lean partial function that returns a string.
tryGetString :: LeanPartialFn CString -> String
tryGetString = tryPureLeanPartialFn getLeanString

class IsLeanValue v p where
  mkLeanValue :: Ptr p -> IO v

-- | Try to run a Lean partial function that returns a Lean value
-- that will need to be freed.
tryAllocLeanValue :: IsLeanValue a p
                  => LeanPartialFn (Ptr p)
                  -> IO a
tryAllocLeanValue = tryLeanPartialFn mkLeanValue

-- | Try to run a Lean partial function that returns a Lean value
-- that will need to be freed.
--
-- Other than allocating a new value or exception, the function
-- must be pure
tryGetLeanValue :: FunPtr (Ptr a -> IO ())
                     -- ^ Pointer to function that releases resource.
                  -> LeanPartialFn (Ptr a)
                  -> ForeignPtr a
tryGetLeanValue free_fn = tryPureLeanPartialFn $ newForeignPtr free_fn
