{-|
Module      : Language.Lean.Internal.Options
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Internal declarations for Lean options and typeclass instances
for @Options@ type.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Lean.Internal.Options
  ( Options
  , emptyOptions
  , joinOptions
    -- * Low level FFI interfaces.
  , OptionsPtr
  , OutOptionsPtr
  , withOptions
  ) where

import Foreign
import Foreign.C
import System.IO.Unsafe

{#import Language.Lean.Internal.Exception #}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"

-- Use nocode and manually generate for haddock.
{#pointer lean_options as Options foreign newtype nocode#}

-- | A set of Lean configuration options
newtype Options = Options (ForeignPtr Options)

-- | Function @c2hs@ uses to pass @Options@ values to Lean
withOptions :: Options -> (Ptr Options -> IO a) -> IO a
withOptions (Options o) = withForeignPtr o

-- | Haskell type for @lean_options@ FFI parameters.
{#pointer lean_options as OptionsPtr -> Options#}
-- | Haskell type for @lean_options*@ FFI parameters.
{#pointer *lean_options as OutOptionsPtr -> OptionsPtr #}

foreign import ccall unsafe "&lean_options_del"
  lean_options_del_ptr :: FunPtr (OptionsPtr -> IO ())

instance IsLeanValue Options (Ptr Options) where
  mkLeanValue = fmap Options . newForeignPtr lean_options_del_ptr

------------------------------------------------------------------------
-- Monoid instance

instance Monoid Options where
  mempty  = emptyOptions
  mappend = joinOptions

-- | An empty set of options
emptyOptions :: Options
emptyOptions = tryGetLeanValue lean_options_mk_empty

-- | Combine two options where the assignments from the second
-- argument override the assignments from the first.
joinOptions :: Options -> Options -> Options
joinOptions x y = tryGetLeanValue $ lean_options_join x y

{#fun unsafe lean_options_mk_empty
  { `OutOptionsPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_join
  { `Options'
  , `Options'
  , `OutOptionsPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

------------------------------------------------------------------------
-- Eq instance

instance Eq Options where
  (==) = lean_options_eq

{#fun pure unsafe lean_options_eq
  { `Options', `Options' } -> `Bool' #}

------------------------------------------------------------------------
-- Show instance

instance Show Options where
  show = showOption

showOption :: Options -> String
showOption x = tryGetLeanValue $ lean_options_to_string x

{#fun unsafe lean_options_to_string
  { `Options'
  , id `Ptr CString'
  , `OutExceptionPtr'
  } -> `Bool' #}
