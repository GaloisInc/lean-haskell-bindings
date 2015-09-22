{-|
Module      : Language.Lean.Options
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Operations for Lean options
-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
module Language.Lean.Options
  ( Options
  , emptyOptions
  , joinOptions
  , nullOptions
  , containsOption
  , boolOption
  , doubleOption
  , intOption
  , uintOption
  , stringOption
  ) where

import Control.Exception (throw)
import Control.Lens
import Foreign
import Foreign.C
import System.IO.Unsafe

{#import Language.Lean.Internal.Exception #}
import Language.Lean.Internal.Exception.Unsafe
{#import Language.Lean.Internal.Name #}
{#import Language.Lean.Internal.String #}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"

{#fun unsafe lean_options_set_bool
  { `Options'
  , `Name'
  , `Bool'
  , id `Ptr OptionsPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_set_int
  { `Options'
  , `Name'
  , `Int32'
  , id `Ptr OptionsPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_set_unsigned
  { `Options'
  , `Name'
  , `Word32'
  , id `Ptr OptionsPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_set_double
  { `Options'
  , `Name'
  , `Double'
  , id `Ptr OptionsPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_set_string
  { `Options'
  , `Name'
  , withLeanStringPtr* `String'
  , id `Ptr OptionsPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Returns true if options are empty.
{#fun pure unsafe lean_options_empty as nullOptions
  { `Options'
  } -> `Bool' #}

-- | Indicate whether name is set in the lean options.
{#fun pure unsafe lean_options_contains as containsOption
  { `Options'
  , `Name'
  } -> `Bool' #}

{#fun unsafe lean_options_get_bool
  { `Options'
  , `Name'
  , id `Ptr CInt'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_get_int
  { `Options'
  , `Name'
  , id `Ptr CInt'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_get_unsigned
  { `Options'
  , `Name'
  , id `Ptr CUInt'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_get_double
  { `Options'
  , `Name'
  , id `Ptr CDouble'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_get_string
  { `Options'
  , `Name'
  , id `Ptr CString'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Lens for getting and setting boolean options without
--   rewriting equivalent values
simpleLensEq :: forall a p
              . (IsLeanValue a p, Eq a)
             => (Options -> Name -> LeanPartialFn p)
             -> (Options -> Name -> a -> LeanPartialFn OptionsPtr)
             -> Name
             -> Simple Lens Options a
simpleLensEq leanGetter leanSetter nm f o = fmap setFun (f oldVal)
  where
    has_val = o `containsOption` nm
    -- This will only throw an error if f demands oldVal
    oldVal
      | has_val = tryGetLeanValue $ leanGetter o nm
      | otherwise =  throw (leanException LeanOtherException msg)
      where msg = "options object does not contain entry " ++ show nm
    setFun :: a -> Options
    setFun newVal = tryGetLeanValue $ leanSetter o nm newVal
{-# INLINE simpleLensEq #-}

-- | Access the lean option with the given name as a Boolean.
boolOption :: Name -> Simple Lens Options Bool
boolOption = simpleLensEq lean_options_get_bool lean_options_set_bool

-- | Access the lean option with the given name as a signed integer.
intOption :: Name -> Simple Lens Options Int32
intOption = simpleLensEq lean_options_get_int lean_options_set_int

-- | Access the lean option with the given name as an unsigned integer.
uintOption :: Name -> Simple Lens Options Word32
uintOption = simpleLensEq lean_options_get_unsigned lean_options_set_unsigned

-- | Access the lean option with the given name as a floating point value.
doubleOption :: Name -> Simple Lens Options Double
doubleOption = simpleLensEq lean_options_get_double lean_options_set_double

-- | Access the lean option with the given name as a string.
stringOption :: Name -> Simple Lens Options String
stringOption = simpleLensEq lean_options_get_string lean_options_set_string
