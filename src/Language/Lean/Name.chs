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
{#import Language.Lean.Internal.Name #}
{#import Language.Lean.Internal.String #}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"

{#fun unsafe lean_name_mk_anonymous
  { id `Ptr NamePtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_name_mk_str
  { `NamePtr'
  , `CString'
  , id `Ptr NamePtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_name_mk_idx
  { `NamePtr'
  , `CUInt'
  , id `Ptr NamePtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun pure unsafe lean_name_is_anonymous
  { `NamePtr'
  } -> `Bool' #}

{#fun pure unsafe lean_name_is_str
  { `NamePtr'
  } -> `Bool' #}

{#fun pure unsafe lean_name_is_idx
  { `NamePtr'
  } -> `Bool' #}

{#fun unsafe lean_name_get_prefix
  { `NamePtr'
  , id `Ptr NamePtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_name_get_str
  { `NamePtr'
  , id `Ptr CString'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_name_get_idx
  { `NamePtr'
  , id `Ptr CUInt'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

-- | The root "anonymous" name
anonymousName :: Name
anonymousName = unsafePerformIO $
  tryAllocName lean_name_mk_anonymous
{-# NOINLINE anonymousName #-}

-- | Append a string to a name.
strName :: Name -> String -> Name
strName pre r = unsafePerformIO $ do
  withNamePtr pre $ \pre_ptr -> do
    withLeanStringPtr r $ \r_ptr -> do
      tryAllocName (lean_name_mk_str pre_ptr r_ptr)
{-# NOINLINE strName #-}

-- | Append a numeric index to a name.
idxName :: Name -> Word32 -> Name
idxName pre i = unsafePerformIO $ do
  withNamePtr pre $ \pre_ptr -> do
    tryAllocName (lean_name_mk_idx pre_ptr (fromIntegral i))
{-# NOINLINE idxName #-}

-- | A view of head of a lean name.
data NameView
   = AnonymousName
     -- ^ The anonymous name.
   | StringName Name String
     -- ^ A name with a string appended.
   | IndexName Name Word32
     -- ^ A name with a numeric value appended.
  deriving (Show)

-- | View the head of a Lean name.
viewName :: Name -> NameView
viewName nm = unsafePerformIO $ do
  withNamePtr nm $ \name_ptr -> do
    if lean_name_is_anonymous name_ptr then
      return $! AnonymousName
    else if lean_name_is_str name_ptr then do
      ptr <- tryAllocName   $ lean_name_get_prefix name_ptr
      r   <- tryAllocString $ lean_name_get_str    name_ptr
      return $! StringName ptr r
    else assert (lean_name_is_idx name_ptr) $ do
      ptr <- tryAllocName $ lean_name_get_prefix name_ptr
      idx <- tryGetUInt $ lean_name_get_idx name_ptr
      return $! IndexName ptr (fromIntegral idx)
{-# NOINLINE viewName #-}
