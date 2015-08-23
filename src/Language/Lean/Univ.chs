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
{- LANGUAGE CPP #-}
{- LANGUAGE DoAndIfThenElse #-}
{- LANGUAGE FlexibleInstances #-}
{- LANGUAGE ForeignFunctionInterface #-}
{- LANGUAGE GeneralizedNewtypeDeriving #-}
{- LANGUAGE MultiParamTypeClasses #-}
{- LANGUAGE StandaloneDeriving #-}
{- LANGUAGE TypeFamilies #-}
module Language.Lean.Univ
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
  ) where

--import Control.Lens (toListOf)
--import Foreign
--import Foreign.C
--import GHC.Exts
--import System.IO.Unsafe

{#import Language.Lean.Internal.Univ #}
