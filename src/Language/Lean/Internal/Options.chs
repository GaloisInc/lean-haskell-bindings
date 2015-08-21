{-# LANGUAGE ForeignFunctionInterface #-}
module Language.Lean.Internal.Options
  ( Options
  , OptionsPtr
  , tryAllocOptions
  , withOptionsPtr
  ) where

import Foreign
import Foreign.C
import System.IO.Unsafe

{#import Language.Lean.Internal.Exception #}
import Language.Lean.Internal.Utils

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"

newtype Options = Options (ForeignPtr Options)

{#pointer lean_options as OptionsPtr -> Options#}

foreign import ccall "&lean_options_del"
  lean_options_del_ptr :: FunPtr (OptionsPtr -> IO ())

{#fun pure unsafe lean_options_eq
  { `OptionsPtr'
  , `OptionsPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_to_string
  { `OptionsPtr'
  , id `Ptr CString'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

-- | Call a C layer function that attempts to allocate a
-- new options
tryAllocOptions :: (Ptr OptionsPtr -> Ptr ExceptionPtr -> IO Bool)
                -> IO Options
tryAllocOptions mk_options =
  fmap Options $ tryAllocLeanValue lean_options_del_ptr $ mk_options

-- | Run an action with the underlying pointer.
withOptionsPtr :: WithValueFn Options OptionsPtr a
withOptionsPtr (Options x) = withForeignPtr x

instance Eq Options where
  (==) = withBinaryPred withOptionsPtr lean_options_eq
  {-# NOINLINE (==) #-}

showOption :: Options -> String
showOption x = unsafePerformIO $ do
  withOptionsPtr x $ \x_ptr ->
    tryAllocString $ lean_options_to_string x_ptr
{-# NOINLINE showOption #-}

instance Show Options where
  show = showOption
