{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Lean.FFI.Name
  ( Name
  , mkAnonymousName
  , strName
  , idxName
  , NameView(..)
  , viewName
  ) where

import Control.Exception (assert, finally)
import Control.Monad
import Data.Text (Text)
import Foreign
import Foreign.C
import System.IO.Unsafe

{#import Language.Lean.FFI.Exception #}
{#import Language.Lean.FFI.String #}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"

{#pointer lean_name as NamePtr -> Name#}

{#fun unsafe lean_mk_anonymous_name
  { id `Ptr NamePtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_mk_str_name
  { `NamePtr'
  , `CString'
  , id `Ptr NamePtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_mk_idx_name
  { `NamePtr'
  , `CUInt'
  , id `Ptr NamePtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_del_name
  { `NamePtr'
  } -> `()' #}

foreign import ccall "&lean_del_name"
  lean_del_name_ptr :: FunPtr (NamePtr -> IO ())

{#fun pure unsafe lean_is_anonymous_name
  { `NamePtr'
  } -> `Bool' #}

{#fun pure unsafe lean_is_str_name
  { `NamePtr'
  } -> `Bool' #}

{#fun pure unsafe lean_is_idx_name
  { `NamePtr'
  } -> `Bool' #}

{#fun pure unsafe lean_name_eq
  { `NamePtr'
  , `NamePtr'
  } -> `Bool' #}

{#fun unsafe lean_get_name_prefix
  { `NamePtr'
  , id `Ptr NamePtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_get_name_str
  { `NamePtr'
  , id `Ptr CString'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_get_name_idx
  { `NamePtr'
  , id `Ptr CUInt'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_name_to_string
  { `NamePtr'
  , id `Ptr CString'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

-- | A Lean Name
newtype Name = Name (ForeignPtr Name)

tryAllocLeanValue :: FunPtr (Ptr a -> IO ())
                   -> (Ptr (Ptr a) -> Ptr ExceptionPtr -> IO Bool)
                   -> IO (ForeignPtr a)
tryAllocLeanValue free_fn alloc_fn =
  tryPartialLeanFn alloc_fn $ newForeignPtr free_fn

tryAllocName :: (Ptr NamePtr -> Ptr ExceptionPtr -> IO Bool)
           -> IO Name
tryAllocName mk_name =
  fmap Name $ tryAllocLeanValue lean_del_name_ptr $ mk_name


withNamePtr :: Name -> (NamePtr -> IO a) -> IO a
withNamePtr (Name nm) = withForeignPtr nm

mkAnonymousName :: IO Name
mkAnonymousName = do
  tryAllocName lean_mk_anonymous_name

strName :: Name -> String -> Name
strName pre r = unsafePerformIO $ do
  withNamePtr pre $ \pre_ptr -> do
    withLeanStringPtr r $ \r_ptr -> do
      tryAllocName (lean_mk_str_name pre_ptr r_ptr)
{-# NOINLINE strName #-}

idxName :: Name -> CUInt -> Name
idxName pre i = unsafePerformIO $ do
  withNamePtr pre $ \pre_ptr -> do
     tryAllocName (lean_mk_idx_name pre_ptr i)
{-# NOINLINE idxName #-}

data NameView
   = AnonymousName
   | StringName Name String
   | IndexName Name CUInt

viewName :: Name -> NameView
viewName nm = unsafePerformIO $ do
  withNamePtr nm $ \name_ptr -> do
    if lean_is_anonymous_name name_ptr then
      return $! AnonymousName
    else if lean_is_str_name name_ptr then do
      ptr <- tryAllocName   $ lean_get_name_prefix name_ptr
      r   <- tryAllocString $ lean_name_to_string name_ptr
      return $! StringName ptr r
    else assert (lean_is_idx_name name_ptr) $ do
      ptr <- tryAllocName $ lean_get_name_prefix name_ptr
      idx <- tryGetUInt $ lean_get_name_idx name_ptr
      return $! IndexName ptr idx

eqName :: Name -> Name -> Bool
eqName x y = unsafePerformIO $ do
  withNamePtr x $ \x_ptr -> do
    withNamePtr y $ \y_ptr -> do
      return $! lean_name_eq x_ptr y_ptr
{-# NOINLINE eqName #-}

instance Eq Name where
  (==) = eqName

showName :: Name -> String
showName nm = unsafePerformIO $ do
  withNamePtr nm $ \pre_ptr ->
    tryAllocString $ lean_name_to_string pre_ptr
{-# NOINLINE showName #-}

instance Show Name where
  show = showName


{-
newtype Environment = Environment (ForeignPtr Environment)

{#pointer *lean_environment as EnvironmentPtr -> Environment #}
{#pointer *lean_exception as ExceptionPtr newtype#}

deriving instance Storable ExceptionPtr

{#enum lean_exception_type as ExceptionType { upcaseFirstLetter, underscoreToCase } deriving (Eq)#}

throwLeanException :: ExceptionPtr -> IO a
throwLeanException ex = do
  tp <- lean_get_exception_type ex
  case tp of
    LeanBadAlloc -> do
      fail "Memory allocation in Lean failed"

tryAllocLeanValue :: (ForeignPtr a -> r)
                   -> FunPtr (Ptr a -> IO ())
                   -> (Ptr (Ptr a) -> Ptr ExceptionPtr -> IO Bool)
                   -> IO r
tryAllocLeanValue ctor free_fn f =
  alloca $ \env_ptr ->do
    alloca $ \ex_ptr ->do
      success <- f env_ptr ex_ptr
      when (not success) $ do
        ex <- peek ex_ptr
        throwLeanException ex
          `finally` lean_free_exception ex
      env <- peek env_ptr
      ctor `fmap` newForeignPtr free_fn env

{#fun unsafe lean_new_environment as lean_new_environment'
  { `CUInt'
  , `Bool'
  , `Bool'
  , `Bool'
  , id `Ptr EnvironmentPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

lean_new_environment :: CUInt -> Bool -> Bool -> Bool -> Environment
lean_new_environment a b c d = unsafePerformIO $
  tryAllocLeanValue Environment lean_free_environment_ptr $
    lean_new_environment' a b c d


foreign import ccall "&lean_free_environment"
  lean_free_environment_ptr :: FunPtr (EnvironmentPtr -> IO ())

{#fun unsafe lean_free_environment
  { `EnvironmentPtr'
  } -> `()' #}

{#fun unsafe lean_free_exception
  { `ExceptionPtr'
  } -> `()' #}

{#fun unsafe lean_get_exception_type
  { `ExceptionPtr'
  } -> `ExceptionType' #}

{#fun unsafe lean_exception_what
  { `ExceptionPtr'
  } -> `CString' #}

--withLeanEnvironment :: (Environment -> IO a) -> IO a
--withLeanEnvironment
-}
