{-|
Module      : Language.Lean.Lean
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com
Stability   : experimental
Portability : POSIX

Operations for working with Lean names.
-}
{-# LANGUAGE Safe #-}
module Language.Lean.Name
  ( Name
  , anonymousName
  , nameAppend
  , nameAppendIndex
  , NameView(..)
  , nameView
  , nameToString
  ) where

import Language.Lean.Internal.Name
