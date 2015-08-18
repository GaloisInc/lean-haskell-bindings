{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Lean.Internal.Exception
  ( LeanException(..)
  , LeanExceptionKind(..)
    -- * FFI types
  , ExceptionPtr
  , throwLeanException
  , tryPartialLeanFn
  , tryAllocString
  , tryGetUInt
  ) where

import Control.Exception (Exception(..), finally, throwIO)
import Control.Monad
import Data.Typeable
import Foreign
import Foreign.C

import Language.Lean.Internal.String

#include "lean_macros.h"
#include "lean_exception.h"

{#pointer lean_exception as ExceptionPtr newtype#}

deriving instance Storable ExceptionPtr

{#fun unsafe lean_del_exception
  { `ExceptionPtr'
  } -> `()' #}

{#fun unsafe lean_get_exception_message
  { id `ExceptionPtr'
  } -> `CString' #}

{#enum lean_exception_kind as ExceptionKind { upcaseFirstLetter }
         deriving (Eq)#}

{#fun pure unsafe lean_get_exception_kind
  { id `ExceptionPtr'
  } -> `ExceptionKind' #}

data LeanExceptionKind
   = LeanSystemException
   | LeanOutOfMemory
   | LeanInterrupted
   | LeanKernelException
   | LeanOtherException
  deriving (Show)

-- | An exception thrown by Lean
data LeanException
   = LeanException !LeanExceptionKind !String
 deriving (Typeable, Show)

instance Exception LeanException

-- | Convert the ExceptionPtr to a Lean exception and throw
-- it while releasing the ExceptionPtr
throwLeanException :: ExceptionPtr -> IO a
throwLeanException ptr = do
  ex <- leanExceptionFromPtr ptr
    `finally` lean_del_exception ptr
  throwIO $ ex

leanExceptionFromPtr :: ExceptionPtr -> IO LeanException
leanExceptionFromPtr ptr = do
  msg <- mkLeanString (lean_get_exception_message ptr)
  return $! LeanException (getLeanExceptionKind ptr) msg

getLeanExceptionKind :: ExceptionPtr -> LeanExceptionKind
getLeanExceptionKind ptr = do
  case lean_get_exception_kind ptr of
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


tryPartialLeanFn :: Storable a
                 => (Ptr a -> Ptr ExceptionPtr -> IO Bool)
                 -> (a -> IO r)
                 -> IO r
tryPartialLeanFn alloc_fn next =
  alloca $ \ret_ptr ->do
    alloca $ \ex_ptr ->do
      success <- alloc_fn ret_ptr ex_ptr
      when (not success) $ do
        ex <- peek ex_ptr
        throwLeanException ex
      next =<< peek ret_ptr


tryAllocString :: (Ptr CString -> Ptr ExceptionPtr -> IO Bool)
               -> IO String
tryAllocString mk_string =
  tryPartialLeanFn mk_string $ \ptr -> do
     decodeLeanString ptr `finally` lean_del_string ptr

tryGetUInt :: (Ptr CUInt -> Ptr ExceptionPtr -> IO Bool)
           -> IO CUInt
tryGetUInt mk_uint =
  tryPartialLeanFn mk_uint $ return
