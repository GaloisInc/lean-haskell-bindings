{-|
Module      : Language.Lean.List
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Declares a data family @List@ for associating a list type
to different Lean values, and provides a typeclass
along with associated operations for constructing and
viewing Lean lists.
-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Trustworthy #-}
module Language.Lean.List
  ( List
  , IsListIso(..)
  , ListView(..)
  , fromListDefault
  , concatList
  , mapList
  , traverseList
    -- * Re-exports
  , GHC.Exts.IsList(..)
  ) where

import Control.Lens (Traversal, over)
import GHC.Exts (IsList(..))

-- | A type family for mapping a Lean value to the internal list
-- representation.
data family List a

-- | View of the front of a list
data ListView l a
   = Nil
   | a :< l

-- | A typeclass for types that are isomorphic to lists.
--
-- This is used to provide functions for manipulating
-- Lean's internal lists without the overhead of converting
-- to and from Haskell lists.
class IsList l => IsListIso l where
  -- | The empty list
  nil :: l
  -- | Cons an element to the front of a list.
  (<|) :: Item l -> l -> l
  -- | View the front of a list.
  listView :: l -> ListView l (Item l)

-- | Convert a ordinary Haskell list to an opague list
fromListDefault :: IsListIso l => [Item l] -> l
fromListDefault [] = nil
fromListDefault (h:r) = h <| fromListDefault r
{-# INLINABLE fromListDefault #-}

-- | Concatenate two lists
concatList :: IsListIso l => l -> l -> l
concatList x y =
  case listView x of
    Nil -> y
    a :< r -> a <| concatList r y

-- | Apply a function to map one list to another.
mapList :: (IsListIso s, IsListIso t)
        => (Item s -> Item t)
        -> s
        -> t
mapList = over traverseList
{-# INLINABLE mapList #-}

-- | A traversal of the elements in a list.
traverseList :: (IsListIso s, IsListIso t)
             => Traversal s t (Item s) (Item t)
traverseList f l =
  case listView l of
   Nil -> pure nil
   h :< r -> (<|) <$> f h <*> traverseList f r
{-# INLINABLE traverseList #-}
