{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Lean.Options
  ( Options
  , emptyOptions
  , optionsContains
  , boolOption
  , OptionsPtr
  , withOptionsPtr
  ) where

import Control.Exception (assert)
import Control.Lens
import Foreign
import Foreign.C
import System.IO.Unsafe

import Language.Lean.Internal.Utils

{#import Language.Lean.Internal.Exception #}
{#import Language.Lean.Internal.String #}
{#import Language.Lean.Name #}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"

{#pointer lean_options as OptionsPtr -> Options#}

{#fun unsafe lean_options_mk_empty
  { id `Ptr OptionsPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_set_bool
  { `OptionsPtr'
  , `NamePtr'
  , `Bool'
  , id `Ptr OptionsPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_set_int
  { `OptionsPtr'
  , `NamePtr'
  , `CInt'
  , id `Ptr OptionsPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_set_unsigned
  { `OptionsPtr'
  , `NamePtr'
  , `CUInt'
  , id `Ptr OptionsPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_set_double
  { `OptionsPtr'
  , `NamePtr'
  , `CDouble'
  , id `Ptr OptionsPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_set_string
  { `OptionsPtr'
  , `NamePtr'
  , `CString'
  , id `Ptr OptionsPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_join
  { `OptionsPtr'
  , `OptionsPtr'
  , id `Ptr OptionsPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_to_string
  { `OptionsPtr'
  , id `Ptr CString'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun pure unsafe lean_options_eq
  { `OptionsPtr'
  , `OptionsPtr'
  } -> `Bool' #}

{#fun pure unsafe lean_options_empty
  { `OptionsPtr'
  } -> `Bool' #}

{#fun pure unsafe lean_options_contains
  { `OptionsPtr'
  , `NamePtr'
  } -> `Bool' #}

{#fun unsafe lean_options_get_bool
  { `OptionsPtr'
  , `NamePtr'
  , id `Ptr CInt'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_get_int
  { `OptionsPtr'
  , `NamePtr'
  , id `Ptr CInt'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_get_unsigned
  { `OptionsPtr'
  , `NamePtr'
  , id `Ptr CUInt'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_get_double
  { `OptionsPtr'
  , `NamePtr'
  , id `Ptr CDouble'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_get_string
  { `OptionsPtr'
  , `NamePtr'
  , id `Ptr CString'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

foreign import ccall "&lean_options_del"
  lean_options_del_ptr :: FunPtr (OptionsPtr -> IO ())

newtype Options = Options (ForeignPtr Options)

-- | Run an action with the underlying pointer.
withOptionsPtr :: WithValueFn Options OptionsPtr a
withOptionsPtr (Options x) = withForeignPtr x

-- | Call a C layer function that attempts to allocate a
-- new options
tryAllocOptions :: (Ptr OptionsPtr -> Ptr ExceptionPtr -> IO Bool)
                -> IO Options
tryAllocOptions mk_options =
  fmap Options $ tryAllocLeanValue lean_options_del_ptr $ mk_options

emptyOptions :: Options
emptyOptions = unsafePerformIO $
  tryAllocOptions lean_options_mk_empty
{-# NOINLINE emptyOptions #-}

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

optionsContains :: Options -> Name -> Bool
optionsContains o nm = unsafePerformIO $ do
  withOptionsPtr o $ \o_ptr ->
    withNamePtr nm $ \nm_ptr ->
      return $ lean_options_contains o_ptr nm_ptr
{-# NOINLINE optionsContains #-}


optionsGetBool :: Options -> Name -> Bool
optionsGetBool o nm = unsafePerformIO $ do
  withOptionsPtr o $ \o_ptr ->
    withNamePtr nm $ \nm_ptr ->
      tryGetBool $ lean_options_get_bool o_ptr nm_ptr
{-# NOINLINE optionsGetBool #-}

optionsSetBool :: Options -> Name -> Bool -> Options
optionsSetBool o nm v = unsafePerformIO $ do
  withOptionsPtr o $ \o_ptr ->
    withNamePtr nm $ \nm_ptr ->
      tryAllocOptions $ lean_options_set_bool o_ptr nm_ptr v
{-# NOINLINE optionsSetBool #-}

boolOption :: Name -> Simple Lens Options Bool
boolOption name = lens (`optionsGetBool` name) (`optionsSetBool` name)
