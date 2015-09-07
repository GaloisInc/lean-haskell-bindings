{-|
Module      : Language.Lean.Internal.Univ
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Internal declarations for Lean names
together with typeclass instances for @Name@.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_HADDOCK not-home #-}
module Language.Lean.Internal.Name
  ( Name
  , anonymousName
  , nameAppend
  , nameAppendIndex
  , NameView(..)
  , nameView
  , nameToString
    -- * Internal declarations
  , NamePtr
  , OutNamePtr
  , withName
  , ListName
  , ListNamePtr
  , OutListNamePtr
  , withListName
  ) where

import Control.Exception (assert, throw)
import Control.Lens (toListOf)
import Data.Char (isDigit)
import Data.String
import Foreign
import Foreign.C
import System.IO.Unsafe

import Language.Lean.List

{#import Language.Lean.Internal.Exception #}
import Language.Lean.Internal.Exception.Unsafe
{#import Language.Lean.Internal.String #}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"

{#pointer  lean_name as Name foreign newtype nocode#}

-- | A Lean name
newtype Name = Name (ForeignPtr Name)

-- | Function @c2hs@ uses to pass @Name@ values to Lean
withName :: Name -> (Ptr Name -> IO a) -> IO a
withName (Name fo) = withForeignPtr $! fo
{-# INLINE withName #-}

-- | Haskell type for @lean_name@ FFI parameters.
{#pointer  lean_name as NamePtr -> Name#}
-- | Haskell type for @lean_name*@ FFI parameters.
{#pointer *lean_name as OutNamePtr -> NamePtr #}

foreign import ccall unsafe "&lean_name_del"
  lean_name_del_ptr :: FunPtr (NamePtr -> IO ())

instance IsLeanValue Name (Ptr Name) where
  mkLeanValue v = Name <$> newForeignPtr lean_name_del_ptr v

instance Eq Name where
  (==) = lean_name_eq

{#fun pure unsafe lean_name_eq { `Name' , `Name' } -> `Bool' #}

instance Ord Name where
   x <= y = not (lean_name_quick_lt y x)

{#fun pure unsafe lean_name_quick_lt { `Name' , `Name' } -> `Bool' #}

-- | Return a name as a string with subnames separated by periods.
nameToString :: Name -> String
nameToString nm = tryGetLeanValue $ lean_name_to_string nm

instance Show Name where
  show = show . nameToString

{#fun unsafe lean_name_to_string
 { `Name', id `Ptr CString', `OutExceptionPtr' } -> `Bool' #}

------------------------------------------------------------------------
-- Constructing Names

-- | The root "anonymous" name
anonymousName :: Name
anonymousName = tryGetLeanValue lean_name_mk_anonymous

-- | Append a string to a name.
nameAppend :: Name -> String -> Name
nameAppend pre r = tryGetLeanValue (lean_name_mk_str pre r)

-- | Append a numeric index to a name.
nameAppendIndex :: Name -> Word32 -> Name
nameAppendIndex pre i = tryGetLeanValue (lean_name_mk_idx pre i)

{#fun unsafe lean_name_mk_anonymous
  { `OutNamePtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_name_mk_str
  { `Name'
  , withLeanStringPtr* `String'
  , `OutNamePtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_name_mk_idx
  { `Name'
  , `Word32'
  , `OutNamePtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

------------------------------------------------------------------------
-- Viewing Names

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
nameView :: Name -> NameView
nameView nm =
  if lean_name_is_anonymous nm then
    AnonymousName
  else if lean_name_is_str nm then do
    StringName (tryGetLeanValue $ lean_name_get_prefix nm)
               (tryGetLeanValue $ lean_name_get_str nm)
  else assert (lean_name_is_idx nm) $ do
    IndexName (tryGetLeanValue $ lean_name_get_prefix nm)
              (tryGetLeanValue $ lean_name_get_idx nm)

{#fun pure unsafe lean_name_is_anonymous { `Name' } -> `Bool' #}
{#fun pure unsafe lean_name_is_str       { `Name' } -> `Bool' #}
{#fun pure unsafe lean_name_is_idx       { `Name' } -> `Bool' #}

{#fun unsafe lean_name_get_prefix
  { `Name', `OutNamePtr', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_name_get_str
  { `Name', id `Ptr CString', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_name_get_idx
  { `Name', id `Ptr CUInt', `OutExceptionPtr' } -> `Bool' #}

------------------------------------------------------------------------
-- Name IsString instance

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
          [(i,"")] -> nameAppendIndex nm i
          _        -> throw $ leanKernelException msg
            where
              msg = "Identifiers cannot begin with a digit."
      go' _ "" = throw $ leanKernelException "Identifiers cannot be empty"
      go' nm s = nameAppend nm s

------------------------------------------------------------------------
-- Name Monoid instance

-- Monoid instance allows one to write code like @nm <> "foo.bar"@ to append
-- "foo.bar" to a name.
instance Monoid Name where
  mempty  = anonymousName
  mappend x y =
    case nameView y of
      AnonymousName   -> x
      StringName yn s -> mappend x yn `nameAppend` s
      IndexName  yn i -> mappend x yn `nameAppendIndex` i

------------------------------------------------------------------------
-- Name Lists

-- | A list of names (constructor not actually exported)
newtype instance List Name = ListName (ForeignPtr (List Name))

-- | A list of Lean universe levels.
{#pointer lean_list_name as ListName foreign newtype nocode#}
-- | Haskell type for @lean_list_name@ FFI parameters.
{#pointer lean_list_name as ListNamePtr -> ListName #}
-- | Haskell type for @lean_list_name*@ FFI parameters.
{#pointer *lean_list_name as OutListNamePtr -> ListNamePtr #}

-- | Synonym for @List Expr@ that can be used in @c2hs@ bindings.
type ListName = List Name

-- | Function @c2hs@ uses to pass @ListName@ values to Lean
withListName :: ListName -> (Ptr ListName -> IO a) -> IO a
withListName (ListName p) = withForeignPtr $! p

instance IsLeanValue (List Name) (Ptr (List Name)) where
  mkLeanValue = fmap ListName . newForeignPtr lean_list_name_del_ptr

foreign import ccall unsafe "&lean_list_name_del"
  lean_list_name_del_ptr :: FunPtr (ListNamePtr -> IO ())

------------------------------------------------------------------------
-- ListName Eq instance

instance Eq (List Name) where
  (==) = lean_list_name_eq

{#fun pure unsafe lean_list_name_eq
   { `ListName'
   , `ListName'
   } -> `Bool' #}

------------------------------------------------------------------------
-- ListName IsListIso instance

instance IsListIso (List Name) where
  nil = tryGetLeanValue $ lean_list_name_mk_nil
  h <| r = tryGetLeanValue $ lean_list_name_mk_cons h r

  listView l =
    if lean_list_name_is_cons l then
      tryGetLeanValue (lean_list_name_head l)
        :< tryGetLeanValue (lean_list_name_tail l)
    else
      Nil

{#fun unsafe lean_list_name_mk_nil
   { `OutListNamePtr'
   , `OutExceptionPtr'
   } -> `Bool' #}

{#fun unsafe lean_list_name_mk_cons
   { `Name'
   , `ListName'
   , `OutListNamePtr'
   , `OutExceptionPtr'
   } -> `Bool' #}

{#fun pure unsafe lean_list_name_is_cons
   { `ListName'
   } -> `Bool' #}

{#fun unsafe lean_list_name_head
   { `ListName'
   , `OutNamePtr'
   , `OutExceptionPtr'
   } -> `Bool' #}

{#fun unsafe lean_list_name_tail
   { `ListName'
   , `OutListNamePtr'
   , `OutExceptionPtr'
   } -> `Bool' #}

------------------------------------------------------------------------
-- ListName IsList instance

instance IsList (List Name) where
  type Item ListName = Name
  fromList = fromListDefault
  toList = toListOf traverseList

------------------------------------------------------------------------
-- ListName Show instance

instance Show (List Name) where
  showsPrec _ l = showList (toList l)
