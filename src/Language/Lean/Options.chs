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
  ) where

import Control.Exception (assert)
import Control.Lens
import Foreign
import Foreign.C
import System.IO.Unsafe

{#import Language.Lean.Internal.Exception #}
{#import Language.Lean.Internal.Name #}
{#import Language.Lean.Internal.Options #}
{#import Language.Lean.Internal.String #}
import Language.Lean.Internal.Utils

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"

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

emptyOptions :: Options
emptyOptions = unsafePerformIO $
  tryAllocOptions lean_options_mk_empty
{-# NOINLINE emptyOptions #-}

optionsContains :: Options -> Name -> Bool
optionsContains o nm = unsafePerformIO $ do
  withOptionsPtr o $ \o_ptr ->
    withNamePtr nm $ \nm_ptr ->
      return $ lean_options_contains o_ptr nm_ptr
{-# NOINLINE optionsContains #-}

-- | Retrieves a value for a Lean option
optionsGet :: (LeanPartialFn a -> IO b)
           -> (OptionsPtr -> NamePtr -> LeanPartialFn a)
           -> Options
           -> Name
           -> b
optionsGet tryGetVal leanGetter o nm = unsafePerformIO $ do
  withOptionsPtr o $ \o_ptr ->
    withNamePtr nm $ \nm_ptr ->
      tryGetVal $ leanGetter o_ptr nm_ptr

-- | Sets a Lean option with a new value
optionsSet :: (OptionsPtr -> NamePtr -> a -> LeanPartialFn OptionsPtr)
           -> Options
           -> Name
           -> a
           -> Options
optionsSet leanSetter o nm v = unsafePerformIO $ do
  withOptionsPtr o $ \o_ptr ->
    withNamePtr nm $ \nm_ptr ->
      tryAllocOptions $ leanSetter o_ptr nm_ptr v

-- | Lens for getting and setting boolean options without
--   rewriting equivalent values
simpleLensEq :: (Eq a) => (s -> a) -> (s -> a -> s) -> Simple Lens s a
simpleLensEq getter setter f o = fmap setFun (f oldVal)
  where
    oldVal = getter o
    setFun newVal
     | oldVal == newVal = o
     | otherwise        = setter o newVal

boolOption :: Name -> Simple Lens Options Bool
boolOption nm = simpleLensEq (`optionsGetBool` nm) (`optionsSetBool` nm)
  where
    optionsGetBool = optionsGet tryGetBool lean_options_get_bool
    optionsSetBool = optionsSet lean_options_set_bool
{-# NOINLINE boolOption #-}
