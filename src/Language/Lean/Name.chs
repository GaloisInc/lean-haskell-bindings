{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Lean.Name
  ( Name
  , anonymousName
  , strName
  , idxName
  , NameView(..)
  , viewName
  ) where

import Control.Exception (assert)
import Foreign
import Foreign.C
import System.IO.Unsafe


{#import Language.Lean.Internal.Exception #}
{#import Language.Lean.Internal.String #}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"

tryAllocLeanValue :: FunPtr (Ptr a -> IO ())
                   -> (Ptr (Ptr a) -> Ptr ExceptionPtr -> IO Bool)
                   -> IO (ForeignPtr a)
tryAllocLeanValue free_fn alloc_fn =
  tryPartialLeanFn alloc_fn $ newForeignPtr free_fn

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

{-
{#fun unsafe lean_del_name
  { `NamePtr'
  } -> `()' #}
-}

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

-- | A Lean name is a c
newtype Name = Name (ForeignPtr Name)

-- | Call a C layer function that attempts to allocate a
-- new name.
tryAllocName :: (Ptr NamePtr -> Ptr ExceptionPtr -> IO Bool)
           -> IO Name
tryAllocName mk_name =
  fmap Name $ tryAllocLeanValue lean_del_name_ptr $ mk_name

-- | Run an action with the underlying name pointer.
withNamePtr :: Name -> (NamePtr -> IO a) -> IO a
withNamePtr (Name nm) = withForeignPtr nm

-- | The root "anonymous" name
anonymousName :: Name
anonymousName = unsafePerformIO $
  tryAllocName lean_mk_anonymous_name
{-# NOINLINE anonymousName #-}

-- | Append a string to a name.
strName :: Name -> String -> Name
strName pre r = unsafePerformIO $ do
  withNamePtr pre $ \pre_ptr -> do
    withLeanStringPtr r $ \r_ptr -> do
      tryAllocName (lean_mk_str_name pre_ptr r_ptr)
{-# NOINLINE strName #-}

-- | Append a numeric index to a name.
idxName :: Name -> Word32 -> Name
idxName pre i = unsafePerformIO $ do
  withNamePtr pre $ \pre_ptr -> do
    tryAllocName (lean_mk_idx_name pre_ptr (fromIntegral i))
{-# NOINLINE idxName #-}

data NameView
   = AnonymousName
     -- ^ The anonymous name.
   | StringName Name String
     -- ^ A name with a string appended.
   | IndexName Name Word32
     -- ^ A name with a numeric value appended.
  deriving (Show)

viewName :: Name -> NameView
viewName nm = unsafePerformIO $ do
  withNamePtr nm $ \name_ptr -> do
    if lean_is_anonymous_name name_ptr then
      return $! AnonymousName
    else if lean_is_str_name name_ptr then do
      ptr <- tryAllocName   $ lean_get_name_prefix name_ptr
      r   <- tryAllocString $ lean_get_name_str    name_ptr
      return $! StringName ptr r
    else assert (lean_is_idx_name name_ptr) $ do
      ptr <- tryAllocName $ lean_get_name_prefix name_ptr
      idx <- tryGetUInt $ lean_get_name_idx name_ptr
      return $! IndexName ptr (fromIntegral idx)
{-# NOLINE viewName #-}

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
