{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Lean.List
  ( List
  , IsListIso(..)
  , ListView(..)
  , fromListDefault
  , concatList
  , mapList
  , traverseList
  ) where

import Control.Lens (Traversal, over)

-- | A type family for mapping a Lean value to the internal list
-- representation.
data family List a

-- | View of the front of a list
data ListView l a
   = Nil
   | a :< l

-- | A typeclass for types that are isomorphic to lists.
class IsListIso l a | l -> a where
  -- | The empty list
  nil :: l
  -- | Cons an element to the front of a list.
  (<|) :: a -> l -> l
  -- | View the front of a list.
  viewList :: l -> ListView l a

-- | Convert a ordinary Haskell list to an opague list
fromListDefault :: IsListIso l a => [a] -> l
fromListDefault [] = nil
fromListDefault (h:r) = h <| fromListDefault r
{-# INLINABLE fromListDefault #-}

-- | Concatenate two lists
concatList :: IsListIso l a => l -> l -> l
concatList x y =
  case viewList x of
    Nil -> y
    a :< r -> a <| concatList r y

-- | Apply a function to map one list to another.
mapList :: (IsListIso s a, IsListIso t b)
        => (a -> b)
        -> s
        -> t
mapList = over traverseList
{-# INLINABLE mapList #-}

-- | A traversal of the elements in a list.
traverseList :: (IsListIso s a, IsListIso t b)
             => Traversal s t a b
traverseList f l =
  case viewList l of
   Nil -> pure nil
   h :< r -> (<|) <$> f h <*> traverseList f r
{-# INLINABLE traverseList #-}
