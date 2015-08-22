{-# LANGUAGE ForeignFunctionInterface #-}
module Language.Lean.Internal.Name
  ( Name
  , anonymousName
  , strName
  , idxName
  , NameView(..)
  , viewName
    -- * Internal declarations
  , NamePtr
  , tryAllocName
  , withName
  ) where

import Control.Exception (assert, throw)
import Data.Char (isDigit)
import Data.String
import Foreign
import Foreign.C
import System.IO.Unsafe

{#import Language.Lean.Internal.Exception #}
{#import Language.Lean.Internal.String #}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"

-- | A Lean name
{#pointer lean_name as Name foreign newtype#}

{#pointer lean_name as NamePtr -> Name#}

foreign import ccall "&lean_name_del"
  lean_name_del_ptr :: FunPtr (NamePtr -> IO ())

-- | Call a C layer function that attempts to allocate a
-- new name.
tryAllocName :: LeanPartialFn NamePtr -> Name
tryAllocName mk_name =
  Name $ tryAllocLeanValue lean_name_del_ptr $ mk_name

{#fun pure unsafe lean_name_eq { `Name' , `Name' } -> `Bool' #}

{#fun unsafe lean_name_to_string
  { `Name'
  , id `Ptr CString'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_name_mk_anonymous
  { id `Ptr NamePtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_name_mk_str
  { `Name'
  , withLeanStringPtr* `String'
  , id `Ptr NamePtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_name_mk_idx
  { `Name'
  , `Word32'
  , id `Ptr NamePtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun pure unsafe lean_name_is_anonymous { `Name' } -> `Bool' #}
{#fun pure unsafe lean_name_is_str       { `Name' } -> `Bool' #}
{#fun pure unsafe lean_name_is_idx       { `Name' } -> `Bool' #}

{#fun unsafe lean_name_get_prefix
  { `Name'
  , id `Ptr NamePtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_name_get_str
  { `Name'
  , id `Ptr CString'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_name_get_idx
  { `Name'
  , id `Ptr CUInt'
  , `OutExceptionPtr'
  } -> `Bool' #}

instance Eq Name where
  (==) = lean_name_eq

instance Show Name where
  show nm = tryAllocString $ lean_name_to_string nm

-- | The root "anonymous" name
anonymousName :: Name
anonymousName = tryAllocName lean_name_mk_anonymous

-- | Append a string to a name.
strName :: Name -> String -> Name
strName pre r = tryAllocName (lean_name_mk_str pre r)

-- | Append a numeric index to a name.
idxName :: Name -> Word32 -> Name
idxName pre i = tryAllocName (lean_name_mk_idx pre i)

instance IsString Name where
  fromString = go anonymousName
    where
      go nm "" = nm
      go nm s  =
        case break (== '.') s of
          (h,'.':r) -> go (go' nm h) r
          (h,r)     -> assert (null r) (go' nm h)
      go' nm s@(c:_) | isDigit c =
        case reads s of
          [(i,"")] -> idxName nm i
          _        -> throw $ leanKernelException msg
            where
              msg = "Identifiers cannot begin with a digit."
      go' _ "" = throw $ leanKernelException "Identifiers cannot be empty"
      go' nm s = strName nm s

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
               (tryAllocString $ lean_name_get_str nm)
  else assert (lean_name_is_idx nm) $ do
    IndexName (tryAllocName $ lean_name_get_prefix nm)
              (tryGetUInt $ lean_name_get_idx nm)

instance Monoid Name where
  mempty  = anonymousName
  mappend x y =
    case viewName y of
      AnonymousName   -> x
      StringName yn s -> strName (mappend x yn) s
      IndexName  yn i -> idxName (mappend x yn) i
