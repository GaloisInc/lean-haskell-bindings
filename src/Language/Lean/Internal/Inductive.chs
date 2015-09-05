{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Lean.Internal.Inductive
  ( InductiveType
  , InductiveTypePtr
  , OutInductiveTypePtr
  , withInductiveType
  , ListInductiveType
  , ListInductiveTypePtr
  , OutListInductiveTypePtr
  , withListInductiveType
  , InductiveDecl
  , InductiveDeclPtr
  , OutInductiveDeclPtr
  , withInductiveDecl
  , List(..)
  ) where

import Control.Lens (toListOf)
import Foreign
import Foreign.C
import GHC.Exts (IsList(..))
import Language.Lean.List
import System.IO.Unsafe

{#import Language.Lean.Internal.Exception#}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"
#include "lean_expr.h"
#include "lean_decl.h"
#include "lean_inductive.h"

------------------------------------------------------------------------
-- InductiveType declarations

-- | An Inductive type
{#pointer  lean_inductive_type as InductiveType foreign newtype#}
{#pointer  lean_inductive_type as InductiveTypePtr -> InductiveType#}
{#pointer *lean_inductive_type as OutInductiveTypePtr -> InductiveTypePtr #}

instance IsLeanValue InductiveType (Ptr InductiveType) where
  mkLeanValue = fmap InductiveType . newForeignPtr lean_inductive_type_del_ptr

foreign import ccall unsafe "&lean_inductive_type_del"
  lean_inductive_type_del_ptr :: FunPtr (InductiveTypePtr -> IO ())

------------------------------------------------------------------------
-- List InductiveType declarations

-- | A list of inductive types (constructor not actually exported)
newtype instance List InductiveType = ListInductiveType (ForeignPtr (List InductiveType))

-- | A list of Lean universe levels.
{#pointer lean_list_inductive_type as ListInductiveType foreign newtype nocode#}
{#pointer lean_list_inductive_type as ListInductiveTypePtr -> ListInductiveType #}
{#pointer *lean_list_inductive_type as OutListInductiveTypePtr -> ListInductiveTypePtr #}

-- Synonym for List InductiveType
type ListInductiveType = List InductiveType

withListInductiveType :: List InductiveType -> (Ptr (List InductiveType) -> IO a) -> IO a
withListInductiveType (ListInductiveType p) = withForeignPtr p

instance IsLeanValue (List InductiveType) (Ptr (List InductiveType)) where
  mkLeanValue = fmap ListInductiveType . newForeignPtr lean_list_inductive_type_del_ptr

foreign import ccall unsafe "&lean_list_inductive_type_del"
  lean_list_inductive_type_del_ptr :: FunPtr (ListInductiveTypePtr -> IO ())

------------------------------------------------------------------------
-- List InductiveType Eq instance

instance Eq (List InductiveType) where
  (==) = lean_list_inductive_type_eq

{#fun pure unsafe lean_list_inductive_type_eq
   { `ListInductiveType'
   , `ListInductiveType'
   } -> `Bool' #}

------------------------------------------------------------------------
-- List InductiveType IsListIso instance

instance IsListIso (List InductiveType) InductiveType where
  nil = tryGetLeanValue $ lean_list_inductive_type_mk_nil
  h <| r = tryGetLeanValue $ lean_list_inductive_type_mk_cons h r

  viewList l =
    if lean_list_inductive_type_is_cons l then
      tryGetLeanValue (lean_list_inductive_type_head l)
        :< tryGetLeanValue (lean_list_inductive_type_tail l)
    else
      Nil

{#fun unsafe lean_list_inductive_type_mk_nil
   { `OutListInductiveTypePtr'
   , `OutExceptionPtr'
   } -> `Bool' #}

{#fun unsafe lean_list_inductive_type_mk_cons
   { `InductiveType'
   , `ListInductiveType'
   , `OutListInductiveTypePtr'
   , `OutExceptionPtr'
   } -> `Bool' #}

{#fun pure unsafe lean_list_inductive_type_is_cons
   { `ListInductiveType'
   } -> `Bool' #}

{#fun unsafe lean_list_inductive_type_head
   { `ListInductiveType'
   , `OutInductiveTypePtr'
   , `OutExceptionPtr'
   } -> `Bool' #}

{#fun unsafe lean_list_inductive_type_tail
   { `ListInductiveType'
   , `OutListInductiveTypePtr'
   , `OutExceptionPtr'
   } -> `Bool' #}

------------------------------------------------------------------------
-- List InductiveType IsList instance

instance IsList (List InductiveType) where
  type Item ListInductiveType = InductiveType
  fromList = fromListDefault
  toList = toListOf traverseList

{-
------------------------------------------------------------------------
-- List InductiveType Show instance

instance Show (List InductiveType) where
  showsPrec _ l = showList (toList l)
-}

------------------------------------------------------------------------
-- InductiveDecl declarations

-- | An Inductive type
{#pointer  lean_inductive_decl as InductiveDecl foreign newtype#}
{#pointer  lean_inductive_decl as InductiveDeclPtr -> InductiveDecl#}
{#pointer *lean_inductive_decl as OutInductiveDeclPtr -> InductiveDeclPtr #}

instance IsLeanValue InductiveDecl (Ptr InductiveDecl) where
  mkLeanValue = fmap InductiveDecl . newForeignPtr lean_inductive_decl_del_ptr

foreign import ccall unsafe "&lean_inductive_decl_del"
  lean_inductive_decl_del_ptr :: FunPtr (InductiveDeclPtr -> IO ())
