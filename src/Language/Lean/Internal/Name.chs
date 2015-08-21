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
withNamePtr :: Name -> (NamePtr -> IO a) -> IO a
withNamePtr (Name nm) = withForeignPtr nm

-- | Call a C layer function that attempts to allocate a
-- new name.
tryAllocName :: LeanPartialFn NamePtr -> IO Name
tryAllocName mk_name =
  fmap Name $ tryAllocLeanValue lean_name_del_ptr $ mk_name

{#fun pure unsafe lean_name_eq
  { withNamePtr* `Name'
  , withNamePtr* `Name'
  } -> `Bool' #}

{#fun unsafe lean_name_to_string
  { withNamePtr* `Name'
  , id `Ptr CString'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

instance Eq Name where
  (==) = lean_name_eq

showName :: Name -> String
showName nm = unsafePerformIO $ do
    tryAllocString $ lean_name_to_string nm

instance Show Name where
  show = showName
