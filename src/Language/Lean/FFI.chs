{-# LANGUAGE ForeignFunctionInterface #-}
module Language.Lean.FFI
  ( lean_initialize
  , lean_finalize
  , lean_new_environment
  , lean_free_environment
  ) where

import Foreign
import Foreign.C

#include "lean/c_api.h"

{#pointer *lean_environment as Environment newtype#}

{#fun unsafe lean_initialize {} -> `()' #}
{#fun unsafe lean_finalize   {} -> `()' #}

{#fun unsafe lean_new_environment as lean_new_environment
  { `CUInt'
  , `Bool'
  , `Bool'
  , `Bool'
  } -> `Environment' #}

{#fun unsafe lean_free_environment
  { `Environment'
  } -> `()' #}
