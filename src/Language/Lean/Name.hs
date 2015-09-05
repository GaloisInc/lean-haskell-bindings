{-|
Module      : Language.Lean.Lean
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com
Stability   : experimental
Portability : POSIX

This module provides operations for working with Lean names.
-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Lean.Name
  ( Name
  , anonymousName
  , strName
  , idxName
  , NameView(..)
  , viewName
  ) where

import Language.Lean.Internal.Name
