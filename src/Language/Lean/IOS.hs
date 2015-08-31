{-# LANGUAGE ExplicitNamespaces #-}
module Language.Lean.IOS
  ( IOState
  , type IOStateType(..)
    -- * Stadnard IOState
  , mkStandardIOState
    -- * Buffered IOState
  , mkBufferedIOState
  , getRegularOutput
  , getDiagnosticOutput
  , resetRegularOutput
  , resetDiagnosticOutput
    -- * Operations on IO State
  , IOStateTypeRepr(..)
  , iosTypeRepr
  , iosGetOptions
  , iosSetOptions
    -- * Operations using IOState
  , ppExpr
  ) where

import Language.Lean.Internal.IOS
