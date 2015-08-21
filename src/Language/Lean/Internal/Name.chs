{-# LANGUAGE ForeignFunctionInterface #-}
module Language.Lean.Internal.Name
  ( Name
  , NamePtr
  , tryAllocName
  , withNamePtr
  ) where

import Foreign
import Foreign.C
import System.IO.Unsafe

{#import Language.Lean.Internal.Exception #}
import Language.Lean.Internal.Utils

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"

{#pointer lean_name as NamePtr -> Name#}

foreign import ccall "&lean_name_del"
  lean_name_del_ptr :: FunPtr (NamePtr -> IO ())

-- | A Lean name
newtype Name = Name (ForeignPtr Name)

-- | Run an action with the underlying name pointer.
withNamePtr :: WithValueFn Name NamePtr a
withNamePtr (Name nm) = withForeignPtr nm

-- | Call a C layer function that attempts to allocate a
-- new name.
tryAllocName :: (Ptr NamePtr -> Ptr ExceptionPtr -> IO Bool)
           -> IO Name
tryAllocName mk_name =
  fmap Name $ tryAllocLeanValue lean_name_del_ptr $ mk_name

{#fun pure unsafe lean_name_eq
  { `NamePtr'
  , `NamePtr'
  } -> `Bool' #}

{#fun unsafe lean_name_to_string
  { `NamePtr'
  , id `Ptr CString'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

instance Eq Name where
  (==) = withBinaryPred withNamePtr lean_name_eq
  {-# NOINLINE (==) #-}

showName :: Name -> String
showName nm = unsafePerformIO $ do
  withNamePtr nm $ \pre_ptr ->
    tryAllocString $ lean_name_to_string pre_ptr
{-# NOINLINE showName #-}

instance Show Name where
  show = showName
