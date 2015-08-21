{-# LANGUAGE ForeignFunctionInterface #-}
module Language.Lean.Internal.Options
  ( Options
  , OptionsPtr
  , tryAllocOptions
  , withOptionsPtr
  , emptyOptions
  , joinOptions
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
newtype Options = Options (ForeignPtr Options)

{#pointer lean_options as OptionsPtr -> Options#}

foreign import ccall "&lean_options_del"
  lean_options_del_ptr :: FunPtr (OptionsPtr -> IO ())

{#fun pure unsafe lean_options_eq
  { withOptionsPtr* `Options'
  , withOptionsPtr* `Options'
  } -> `Bool' #}

{#fun unsafe lean_options_to_string
  { withOptionsPtr* `Options'
  , id `Ptr CString'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_mk_empty
  { id `Ptr OptionsPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_join
  { withOptionsPtr* `Options'
  , withOptionsPtr* `Options'
  , id `Ptr OptionsPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}


-- | Call a C layer function that attempts to allocate a
-- new options
tryAllocOptions :: (Ptr OptionsPtr -> Ptr ExceptionPtr -> IO Bool)
                -> Options
tryAllocOptions mk_options = unsafePerformIO $ do
  fmap Options $ tryAllocLeanValue lean_options_del_ptr $ mk_options

-- | Run an action with the underlying pointer.
withOptionsPtr :: Options -> (OptionsPtr -> IO a) -> IO a
withOptionsPtr (Options x) = withForeignPtr x

instance Eq Options where
  (==) = lean_options_eq

showOption :: Options -> String
showOption x = unsafePerformIO $ do
  tryAllocString $ lean_options_to_string x

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
