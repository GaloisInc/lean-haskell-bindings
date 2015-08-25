{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Lean.Internal.Options
  ( Options
  , emptyOptions
  , joinOptions
    -- * Low level FFI interfaces.
  , OptionsPtr
  , OutOptionsPtr
  , tryGetOptions
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

-- | A set of Lean configuration options
{#pointer lean_options as Options foreign newtype#}
{#pointer lean_options as OptionsPtr -> Options#}
{#pointer *lean_options as OutOptionsPtr -> OptionsPtr #}

foreign import ccall "&lean_options_del"
  lean_options_del_ptr :: FunPtr (OptionsPtr -> IO ())

instance IsLeanValue Options Options where
  mkLeanValue = fmap Options . newForeignPtr lean_options_del_ptr

-- | Call a C layer function that attempts to allocate a
-- new options
tryGetOptions :: LeanPartialFn OptionsPtr
                -> Options
tryGetOptions mk_options =
  Options $ tryGetLeanValue lean_options_del_ptr $ mk_options

------------------------------------------------------------------------
-- Monoid instance

instance Monoid Options where
  mempty  = emptyOptions
  mappend = joinOptions

-- | An empty set of options
emptyOptions :: Options
emptyOptions = tryGetOptions lean_options_mk_empty

-- | Combine two options where the assignments from the second
-- argument override the assignments from the first.
joinOptions :: Options -> Options -> Options
joinOptions x y = tryGetOptions $ lean_options_join x y

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
showOption x = tryGetString $ lean_options_to_string x

{#fun unsafe lean_options_to_string
  { `Options'
  , id `Ptr CString'
  , `OutExceptionPtr'
  } -> `Bool' #}
