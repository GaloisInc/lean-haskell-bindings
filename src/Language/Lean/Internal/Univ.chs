{-|
Module      : Language.Lean.Internal.Univ
Description : Internal declarations for Lean Universes
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com
Stability   : experimental
Portability : POSIX

This module defines internal functions for universe levels.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Lean.Internal.Univ
  ( Univ
  , zeroUniv
  , succUniv
  , maxUniv
  , imaxUniv
  , paramUniv
  , globalUniv
  , metaUniv
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
import GHC.Exts
import System.IO.Unsafe

import Language.Lean.List
{#import Language.Lean.Internal.Exception #}
{#import Language.Lean.Internal.Name #}
{#import Language.Lean.Internal.Options #}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"

-- | A Lean universe level
{#pointer lean_univ as Univ foreign newtype#}
{#pointer lean_univ as UnivPtr -> Univ#}
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
-- Operations for constructing universes

-- | The zero universe
zeroUniv :: Univ
zeroUniv = tryGetLeanValue $ lean_univ_mk_zero

{#fun unsafe lean_univ_mk_zero
 { `OutUnivPtr', `OutExceptionPtr' } -> `Bool' #}

-- | Successor of the universe
succUniv :: Univ -> Univ
succUniv x = tryGetLeanValue $ lean_univ_mk_succ x

{#fun unsafe lean_univ_mk_succ
 { `Univ', `OutUnivPtr', `OutExceptionPtr' } -> `Bool' #}

-- | The max of two universes.
maxUniv :: Univ -> Univ -> Univ
maxUniv x y = tryGetLeanValue $ lean_univ_mk_max x y

{#fun unsafe lean_univ_mk_max
 { `Univ', `Univ', `OutUnivPtr', `OutExceptionPtr' } -> `Bool' #}

-- | The imax of two universes.
imaxUniv :: Univ -> Univ -> Univ
imaxUniv x y = tryGetLeanValue $ lean_univ_mk_imax x y

{#fun unsafe lean_univ_mk_imax
 { `Univ', `Univ', `OutUnivPtr', `OutExceptionPtr' } -> `Bool' #}

-- | A universe parameter of the given name.
paramUniv :: Name -> Univ
paramUniv x = tryGetLeanValue $ lean_univ_mk_param x

{#fun unsafe lean_univ_mk_param
 { `Name', `OutUnivPtr', `OutExceptionPtr' } -> `Bool' #}

-- | A global universe with the given name.
globalUniv :: Name -> Univ
globalUniv x = tryGetLeanValue $ lean_univ_mk_global x

{#fun unsafe lean_univ_mk_global
 { `Name', `OutUnivPtr', `OutExceptionPtr' } -> `Bool' #}

-- | A universe meta-variable with the given name.
metaUniv :: Name -> Univ
metaUniv x = tryGetLeanValue $ lean_univ_mk_meta x

{#fun unsafe lean_univ_mk_meta
 { `Name', `OutUnivPtr', `OutExceptionPtr' } -> `Bool' #}

------------------------------------------------------------------------
-- Univ Num instance

-- This instance is only so that we can describe explicit
-- universe levels using integer literals.  All other methods will throw an
-- error if called.
instance Num Univ where
  (+) = error "(+) is undefined on Lean universe levels"
  (-) = error "(-) is undefined on Lean universe levels"
  (*) = error "(*) is undefined on Lean universe levels"
  abs x = x
  signum = error "signum is undefined on Lean universe levels"
  negate = error "Lean universe levels cannot be negated."

  fromInteger i0 | i0 < 0 = error "Universes cannot be negative."
                 | otherwise = go zeroUniv i0
    where -- Make sure first argument is evaluated
          go r _ | seq r False = error "unexpected"
          go r 0 = r
          go r i = go (succUniv r) (i-1)

------------------------------------------------------------------------
-- Univ Lists

-- | Definition for liss of universes.
newtype instance List Univ = ListUniv (ForeignPtr (List Univ))

-- | A list of Lean universe levels.
{#pointer lean_list_univ as ListUniv foreign newtype nocode#}
{#pointer lean_list_univ as ListUnivPtr -> ListUniv #}
{#pointer *lean_list_univ as OutListUnivPtr -> ListUnivPtr #}

-- Code for List Univ
type ListUniv = List Univ

withListUniv :: List Univ -> (Ptr (List Univ) -> IO a) -> IO a
withListUniv (ListUniv p) = withForeignPtr p

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
-- ListUniv IsListIso instance

instance IsListIso (List Univ) Univ where
  nil = tryGetLeanValue $ lean_list_univ_mk_nil
  h <| r = tryGetLeanValue $ lean_list_univ_mk_cons h r

  viewList l =
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
-- ListUniv IsList instance

instance IsList (List Univ) where
  type Item ListUniv = Univ
  fromList = fromListDefault
  toList = toListOf traverseList

------------------------------------------------------------------------
-- ListUniv Show instance

instance Show (List Univ) where
  showsPrec _ l = showList (toList l)
