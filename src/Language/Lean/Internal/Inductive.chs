{-|
Module      : Language.Lean.Inductive
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Internal declarations for inductive types and declarations.
-}
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

{#pointer  lean_inductive_type as InductiveType foreign newtype nocode#}

-- | An inductive type
newtype InductiveType = InductiveType (ForeignPtr InductiveType)

-- | Access raw @lean_inductive_type@ within IO action.
withInductiveType :: InductiveType -> (Ptr InductiveType -> IO a) -> IO a
withInductiveType (InductiveType o) = withForeignPtr o

-- | Haskell type for @lean_inductive_type@ FFI parameters.
{#pointer  lean_inductive_type as InductiveTypePtr -> InductiveType#}
-- | Haskell type for @lean_inductive_type*@ FFI parameters.
{#pointer *lean_inductive_type as OutInductiveTypePtr -> InductiveTypePtr #}

instance IsLeanValue InductiveType (Ptr InductiveType) where
  mkLeanValue = fmap InductiveType . newForeignPtr lean_inductive_type_del_ptr

foreign import ccall unsafe "&lean_inductive_type_del"
  lean_inductive_type_del_ptr :: FunPtr (InductiveTypePtr -> IO ())

------------------------------------------------------------------------
-- List InductiveType declarations

-- | A list of inductive types (constructor not actually exported)
newtype instance List InductiveType = ListInductiveType (ForeignPtr (List InductiveType))

{#pointer lean_list_inductive_type as ListInductiveType foreign newtype nocode#}

-- | Haskell type for @lean_list_inductive_type@ FFI parameters.
{#pointer lean_list_inductive_type as ListInductiveTypePtr -> ListInductiveType #}
-- | Haskell type for @lean_list_inductive_type*@ FFI parameters.
{#pointer *lean_list_inductive_type as OutListInductiveTypePtr -> ListInductiveTypePtr #}

-- | Synonym for @List InductiveType@
type ListInductiveType = List InductiveType

-- | Access raw @lean_list_inductive_type@ within IO action.
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

  listView l =
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

------------------------------------------------------------------------
-- InductiveDecl declarations

-- | An Inductive
{#pointer  lean_inductive_decl as InductiveDecl foreign newtype nocode#}


-- | An inductive declaration
newtype InductiveDecl = InductiveDecl (ForeignPtr InductiveDecl)

-- | Access raw @lean_inductive_decl@ within IO action.
withInductiveDecl :: InductiveDecl -> (Ptr InductiveDecl -> IO a) -> IO a
withInductiveDecl (InductiveDecl o) = withForeignPtr o

-- | Haskell type for @lean_inductive_decl@ FFI parameters.
{#pointer lean_inductive_decl as InductiveDeclPtr -> InductiveDecl #}
-- | Haskell type for @lean_inductive_decl*@ FFI parameters.
{#pointer *lean_inductive_decl as OutInductiveDeclPtr -> InductiveDeclPtr #}

instance IsLeanValue InductiveDecl (Ptr InductiveDecl) where
  mkLeanValue = fmap InductiveDecl . newForeignPtr lean_inductive_decl_del_ptr

foreign import ccall unsafe "&lean_inductive_decl_del"
  lean_inductive_decl_del_ptr :: FunPtr (InductiveDeclPtr -> IO ())
