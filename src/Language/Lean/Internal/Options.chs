{-# LANGUAGE ForeignFunctionInterface #-}
module Language.Lean.Internal.Options
  ( Options
  , emptyOptions
  , joinOptions
    -- * Low level FFI interfaces.
  , OptionsPtr
  , tryAllocOptions
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

foreign import ccall "&lean_options_del"
  lean_options_del_ptr :: FunPtr (OptionsPtr -> IO ())

-- | Call a C layer function that attempts to allocate a
-- new options
tryAllocOptions :: LeanPartialFn OptionsPtr
                -> Options
tryAllocOptions mk_options =
  Options $ tryAllocLeanValue lean_options_del_ptr $ mk_options

{#fun pure unsafe lean_options_eq
  { `Options', `Options' } -> `Bool' #}

{#fun unsafe lean_options_to_string
  { `Options'
  , id `Ptr CString'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_mk_empty
  { id `Ptr OptionsPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_join
  { `Options'
  , `Options'
  , id `Ptr OptionsPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

instance Eq Options where
  (==) = lean_options_eq

showOption :: Options -> String
showOption x = tryAllocString $ lean_options_to_string x

instance Show Options where
  show = showOption

-- | An empty set of options
emptyOptions :: Options
emptyOptions = tryAllocOptions lean_options_mk_empty

-- | Combine two options where the assignments from the second
-- argument override the assignments from the first.
joinOptions :: Options -> Options -> Options
joinOptions x y = tryAllocOptions $ lean_options_join x y

instance Monoid Options where
  mempty  = emptyOptions
  mappend = joinOptions
