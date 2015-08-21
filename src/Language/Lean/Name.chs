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
  { withNamePtr* `Name'
  , withLeanStringPtr* `String'
  , id `Ptr NamePtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_name_mk_idx
  { withNamePtr* `Name'
  , `Word32'
  , id `Ptr NamePtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun pure unsafe lean_name_is_anonymous
  { withNamePtr* `Name'
  } -> `Bool' #}

{#fun pure unsafe lean_name_is_str
  { withNamePtr* `Name'
  } -> `Bool' #}

{#fun pure unsafe lean_name_is_idx
  { withNamePtr* `Name'
  } -> `Bool' #}

{#fun unsafe lean_name_get_prefix
  { withNamePtr* `Name'
  , id `Ptr NamePtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_name_get_str
  { withNamePtr* `Name'
  , id `Ptr CString'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_name_get_idx
  { withNamePtr* `Name'
  , id `Ptr CUInt'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

-- | The root "anonymous" name
anonymousName :: Name
anonymousName = tryAllocName lean_name_mk_anonymous

-- | Append a string to a name.
strName :: Name -> String -> Name
strName pre r = tryAllocName (lean_name_mk_str pre r)

-- | Append a numeric index to a name.
idxName :: Name -> Word32 -> Name
idxName pre i = tryAllocName (lean_name_mk_idx pre i)

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
viewName nm =
  if lean_name_is_anonymous nm then
    AnonymousName
  else if lean_name_is_str nm then do
    StringName (tryAllocName $ lean_name_get_prefix nm)
               (unsafePerformIO $ tryAllocString $ lean_name_get_str nm)
  else assert (lean_name_is_idx nm) $ do
    IndexName (tryAllocName $ lean_name_get_prefix nm)
              (unsafePerformIO $ tryGetUInt $ lean_name_get_idx nm)
