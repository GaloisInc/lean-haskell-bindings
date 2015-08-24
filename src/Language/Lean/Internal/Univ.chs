{-|
Module      : Language.Lean.Univ
Description : Operations on Lean Universes
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com
Stability   : experimental
Portability : POSIX

This module defines functions for constructing and deconstructing lean universes.
-}
{- LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{- LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- LANGUAGE StandaloneDeriving #-}
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
  , UnivView(..)
  , viewUniv
  , geqUniv
  , showUniv
  , showUnivUsing
    -- * Operations on universe levels
  , normalizeUniv
  , instantiateUniv
  , instantiateUniv2
    -- * Internal Operations
  , UnivPtr
  , OutUnivPtr
  , withUniv
  , allocUniv
  , tryAllocUniv
  , ListUniv
  , ListUnivPtr
  , OutListUnivPtr
  , withListUniv
  , tryAllocListUniv
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

{-
type WithPointer b p a = b -> (p -> IO a) -> IO a

withForeignArray :: Storable p => WithPointer b p a -> Int -> [b] -> (Ptr p -> IO a) -> IO a
withForeignArray withPtr n l action = do
  allocaArray n $ \ptr -> do
    withForeignArray' withPtr ptr l action

withForeignArray' :: Storable p => WithPointer b p a -> Ptr p -> [b] -> (Ptr p -> IO a) -> IO a
withForeignArray' _ arrayPtr [] action = action arrayPtr
withForeignArray' withPtr arrayPtr (h:r) action = do
  withPtr h $ \ptr -> do
    poke arrayPtr ptr
    withForeignArray' withPtr (arrayPtr `plusPtr` sizeOf ptr) r action
-}

-- | A Lean universe level
{#pointer lean_univ as Univ foreign newtype#}
{#pointer lean_univ as UnivPtr -> Univ#}
{#pointer *lean_univ as OutUnivPtr -> UnivPtr #}

foreign import ccall "&lean_univ_del"
  lean_univ_del_ptr :: FunPtr (UnivPtr -> IO ())

-- | Create a universe level from a pointer.
allocUniv :: UnivPtr -> IO Univ
allocUniv ptr = Univ <$> newForeignPtr lean_univ_del_ptr ptr

-- | Call a C layer function that attempts to allocate a new universe and is pure.
tryAllocUniv :: LeanPartialFn UnivPtr -> Univ
tryAllocUniv = Univ . tryAllocLeanValue lean_univ_del_ptr

------------------------------------------------------------------------
-- Options for constructing universes

-- | The zero universe
zeroUniv :: Univ
zeroUniv = tryAllocUniv $ lean_univ_mk_zero

{#fun unsafe lean_univ_mk_zero
  { `OutUnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Successor of the universe
succUniv :: Univ -> Univ
succUniv x = tryAllocUniv $ lean_univ_mk_succ x

{#fun unsafe lean_univ_mk_succ
  { `Univ'
  , `OutUnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | The max of two universes.
maxUniv :: Univ -> Univ -> Univ
maxUniv x y = tryAllocUniv $ lean_univ_mk_max x y

{#fun unsafe lean_univ_mk_max
  { `Univ'
  , `Univ'
  , `OutUnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | The imax of two universes.
imaxUniv :: Univ -> Univ -> Univ
imaxUniv x y = tryAllocUniv $ lean_univ_mk_imax x y

{#fun unsafe lean_univ_mk_imax
  { `Univ'
  , `Univ'
  , `OutUnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | A universe parameter of the given name.
paramUniv :: Name -> Univ
paramUniv x = tryAllocUniv $ lean_univ_mk_param x

{#fun unsafe lean_univ_mk_param
  { `Name'
  , `OutUnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | A global universe with the given name.
globalUniv :: Name -> Univ
globalUniv x = tryAllocUniv $ lean_univ_mk_global x

{#fun unsafe lean_univ_mk_global
  { `Name'
  , `OutUnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | A universe meta-variable with the given name.
metaUniv :: Name -> Univ
metaUniv x = tryAllocUniv $ lean_univ_mk_meta x

{#fun unsafe lean_univ_mk_meta
  { `Name'
  , `OutUnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

------------------------------------------------------------------------
-- Equality and comparison of universes.

instance Eq Univ where
  (==) = eqUniv

-- | Return whether two universes are equal.
eqUniv :: Univ -> Univ -> Bool
eqUniv x y = tryGetBool $ lean_univ_eq x y

{#fun unsafe lean_univ_eq
  { `Univ'
  , `Univ'
  , id `Ptr CInt'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | @geqUniv x y@ returns @true@ if @y@ is a larger universe level
-- than @x@ for all possible assignments to the variables in the
-- @x@ and @y@.
geqUniv :: Univ -> Univ -> Bool
geqUniv x y = tryGetBool $ lean_univ_geq x y

{#fun unsafe lean_univ_geq
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
showUniv u = tryAllocString $ lean_univ_to_string u

-- | Show a universe with the given options.
showUnivUsing :: Univ -> Options -> String
showUnivUsing u options = tryAllocString $ lean_univ_to_string_using u options

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
-- View

-- | A view of a universe.
data UnivView
   = UnivZero
     -- ^ The zero universe.
   | UnivSucc !Univ
     -- ^ Successor of the previous universe.
   | UnivMax !Univ !Univ
     -- ^ Maximum of two universes.
   | UnivIMax !Univ !Univ
     -- ^ @UnivIMax x y@ denotes @y@ if @y@ is universe zero, otherwise @UnivMax x y@
   | UnivParam !Name
     -- ^ Universe parameter with the given name.
   | UnivGlobal !Name
     -- ^ Reference to a global universe.
   | UnivMeta !Name
     -- ^ Meta variable with the given name.
  deriving (Show)

-- | Create a view of the universe.
viewUniv :: Univ -> UnivView
viewUniv x =
  case lean_univ_get_kind x of
   LEAN_UNIV_ZERO -> UnivZero
   LEAN_UNIV_SUCC -> UnivSucc (tryAllocUniv $ lean_univ_get_pred x)
   LEAN_UNIV_MAX ->
     UnivMax (tryAllocUniv $ lean_univ_get_max_lhs x)
             (tryAllocUniv $ lean_univ_get_max_rhs x)
   LEAN_UNIV_IMAX ->
     UnivIMax (tryAllocUniv $ lean_univ_get_max_lhs x)
              (tryAllocUniv $ lean_univ_get_max_rhs x)
   LEAN_UNIV_PARAM  -> UnivParam  (tryAllocName $ lean_univ_get_name x)
   LEAN_UNIV_GLOBAL -> UnivGlobal (tryAllocName $ lean_univ_get_name x)
   LEAN_UNIV_META   -> UnivMeta   (tryAllocName $ lean_univ_get_name x)

{#enum lean_univ_kind as UnivKind { upcaseFirstLetter }
         deriving (Eq)#}

{#fun pure unsafe lean_univ_get_kind
  { `Univ'
  } -> `UnivKind' #}

{#fun unsafe lean_univ_get_pred
  { `Univ'
  , `OutUnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_get_max_lhs
  { `Univ'
  , `OutUnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_get_max_rhs
  { `Univ'
  , `OutUnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_get_name
  { `Univ'
  , `OutNamePtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

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

-- | Call a C layer function that attempts to allocate a new universe and is pure.
tryAllocListUniv :: LeanPartialFn ListUnivPtr -> ListUniv
tryAllocListUniv = ListUniv . tryAllocLeanValue lean_list_univ_del_ptr

foreign import ccall "&lean_list_univ_del"
  lean_list_univ_del_ptr :: FunPtr (ListUnivPtr -> IO ())

------------------------------------------------------------------------
-- ListUniv Eq instance

instance Eq (List Univ) where
  x == y = tryGetBool $ lean_list_univ_eq x y

{#fun unsafe lean_list_univ_eq
   { `ListUniv'
   , `ListUniv'
   , id `Ptr CInt'
   , `OutExceptionPtr'
   } -> `Bool' #}

------------------------------------------------------------------------
-- ListUniv IsListIso instance

instance IsListIso (List Univ) Univ where
  nil = tryAllocListUniv $ lean_list_univ_mk_nil
  h <| r = tryAllocListUniv $ lean_list_univ_mk_cons h r

  viewList l =
    if lean_list_univ_is_cons l then
      tryAllocUniv (lean_list_univ_head l)
        :< tryAllocListUniv (lean_list_univ_tail l)
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

------------------------------------------------------------------------
-- Normalize

{#fun unsafe lean_univ_normalize
  { `Univ'
  , `OutUnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Return the normal form for a universe.
normalizeUniv :: Univ -> Univ
normalizeUniv x = tryAllocUniv $ lean_univ_normalize x

------------------------------------------------------------------------
-- Instantiate

-- | Instantiate the parameters with universes
instantiateUniv :: Univ -> [(Name,Univ)] -> Univ
instantiateUniv u bindings =
  instantiateUniv2 u (fromList (fst <$> bindings)) (fromList (snd <$> bindings))

-- | Instantiate the parameters with universes using separate lists for names and levels.
instantiateUniv2 :: Univ
                 -> List Name
                 -> List Univ
                 -> Univ
instantiateUniv2 u nms args = tryAllocUniv $ lean_univ_instantiate u nms args

{#fun unsafe lean_univ_instantiate
  { `Univ'
  , `ListName'
  , `ListUniv'
  , `OutUnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}
