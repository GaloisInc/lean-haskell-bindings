{-|
Module      : Language.Lean.Exception
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Exceptions thrown by Lean.
-}
{-# LANGUAGE Safe #-}
module Language.Lean.Exception
  ( LeanException
  , LeanExceptionKind(..)
  , exceptionKind
  , exceptionMessage
  , exceptionMessageWithEnv
  , exceptionRawMessage
  , exceptionDetailedMessage
  ) where

import Language.Lean.Internal.Exception
