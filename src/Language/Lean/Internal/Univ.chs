{-|
Module      : Language.Lean.Internal.Univ
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Internal declarations for Lean universe values together with typeclass instances
for @Univ@.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Trustworthy #-}
module Language.Lean.Internal.Univ
  ( Univ
  , showUniv
  , showUnivUsing
  , univLt
    -- * Internal Operations
  , UnivPtr
  , OutUnivPtr
  , withUniv
  , ListUniv
  , ListUnivPtr
  , OutListUnivPtr
  , withListUniv
  ) where

import Control.Lens (toListOf)
import Foreign
import Foreign.C
import System.IO.Unsafe

import Language.Lean.List
{#import Language.Lean.Internal.Exception #}
import Language.Lean.Internal.Exception.Unsafe
{#import Language.Lean.Internal.Options #}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"

{#pointer lean_univ as Univ foreign newtype nocode#}

-- | A Lean universe level
newtype Univ = Univ (ForeignPtr Univ)

-- | Function @c2hs@ uses to pass @Univ@ values to Lean
withUniv :: Univ -> (Ptr Univ -> IO a) -> IO a
withUniv (Univ o) = withForeignPtr $! o

-- | Haskell type for @lean_univ@ FFI parameters.
{#pointer lean_univ as UnivPtr -> Univ#}

-- | Haskell type for @lean_univ*@ FFI parameters.
{#pointer *lean_univ as OutUnivPtr -> UnivPtr #}

foreign import ccall unsafe "&lean_univ_del"
  lean_univ_del_ptr :: FunPtr (UnivPtr -> IO ())

instance IsLeanValue Univ (Ptr Univ) where
  mkLeanValue = fmap Univ . newForeignPtr lean_univ_del_ptr

------------------------------------------------------------------------
-- Equality and comparison of universes.

instance Eq Univ where
  x == y = tryGetLeanValue $ lean_univ_eq x y

{#fun unsafe lean_univ_eq
  { `Univ'
  , `Univ'
  , id `Ptr CInt'
  , `OutExceptionPtr'
  } -> `Bool' #}

instance Ord Univ where
  x <= y = not (tryGetLeanValue $ y `lean_univ_quick_lt` x)

{#fun unsafe lean_univ_quick_lt
  { `Univ'
  , `Univ'
  , id `Ptr CInt'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Total ordering over universes using structural equality.
univLt :: Univ -> Univ -> Bool
univLt x y = tryGetLeanValue $ x `lean_univ_lt` y

{#fun unsafe lean_univ_lt
  { `Univ'
  , `Univ'
  , id `Ptr CInt'
  , `OutExceptionPtr'
  } -> `Bool' #}

------------------------------------------------------------------------
-- Univ instance

instance Show Univ where
  show = showUniv

-- | Show a universe.
showUniv :: Univ -> String
showUniv u = tryGetLeanValue $ lean_univ_to_string u

-- | Show a universe with the given options.
showUnivUsing :: Univ -> Options -> String
showUnivUsing u options = tryGetLeanValue $ lean_univ_to_string_using u options

{#fun unsafe lean_univ_to_string
  { `Univ'
  , id `Ptr CString'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_to_string_using
  { `Univ'
  , `Options'
  , id `Ptr CString'
  , `OutExceptionPtr'
  } -> `Bool' #}

------------------------------------------------------------------------
-- Univ Lists

-- | A list of universes (constructor not actually exported)
newtype instance List Univ = ListUniv (ForeignPtr (List Univ))

{#pointer lean_list_univ as ListUniv foreign newtype nocode#}

-- | Haskell type for @lean_list_univ@ FFI parameters.
{#pointer lean_list_univ as ListUnivPtr -> ListUniv #}
-- | Haskell type for @lean_list_univ*@ FFI parameters.
{#pointer *lean_list_univ as OutListUnivPtr -> ListUnivPtr #}

-- | Synonym for @List Expr@ that can be used in @c2hs@ bindings
type ListUniv = List Univ

-- | Function @c2hs@ uses to pass @ListUniv@ values to Lean
withListUniv :: ListUniv -> (Ptr ListUniv -> IO a) -> IO a
withListUniv (ListUniv p) = withForeignPtr $! p

instance IsLeanValue (List Univ) (Ptr (List Univ)) where
  mkLeanValue = fmap ListUniv . newForeignPtr lean_list_univ_del_ptr

foreign import ccall unsafe "&lean_list_univ_del"
  lean_list_univ_del_ptr :: FunPtr (ListUnivPtr -> IO ())

------------------------------------------------------------------------
-- ListUniv Eq instance

instance Eq (List Univ) where
  x == y = tryGetLeanValue $ lean_list_univ_eq x y

{#fun unsafe lean_list_univ_eq
   { `ListUniv'
   , `ListUniv'
   , id `Ptr CInt'
   , `OutExceptionPtr'
   } -> `Bool' #}

------------------------------------------------------------------------
-- ListUniv IsList instance

-- | Allow @(List Univ)@ to use @OverloadedLists@ extensions.
instance IsList (List Univ) where
  -- | List Univ type family instance needed by @IsList (List Univ)@
  type Item (List Univ) = Univ
  fromList = fromListDefault
  toList = toListOf traverseList

------------------------------------------------------------------------
-- ListUniv IsListIso instance

instance IsListIso (List Univ) where
  nil = tryGetLeanValue $ lean_list_univ_mk_nil
  h <| r = tryGetLeanValue $ lean_list_univ_mk_cons h r

  listView l =
    if lean_list_univ_is_cons l then
      tryGetLeanValue (lean_list_univ_head l)
        :< tryGetLeanValue (lean_list_univ_tail l)
    else
      Nil

{#fun unsafe lean_list_univ_mk_nil
   { `OutListUnivPtr'
   , `OutExceptionPtr'
   } -> `Bool' #}

{#fun unsafe lean_list_univ_mk_cons
   { `Univ'
   , `ListUniv'
   , `OutListUnivPtr'
   , `OutExceptionPtr'
   } -> `Bool' #}

{#fun pure unsafe lean_list_univ_is_cons
   { `ListUniv'
   } -> `Bool' #}

{#fun unsafe lean_list_univ_head
   { `ListUniv'
   , `OutUnivPtr'
   , `OutExceptionPtr'
   } -> `Bool' #}

{#fun unsafe lean_list_univ_tail
   { `ListUniv'
   , `OutListUnivPtr'
   , `OutExceptionPtr'
   } -> `Bool' #}

------------------------------------------------------------------------
-- ListUniv Show instance

instance Show (List Univ) where
  showsPrec _ l = showList (toList l)
