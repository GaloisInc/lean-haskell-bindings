{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Lean.Univ
  ( Univ
  , UnivView(..)
  , viewUniv
  , showUniv
  , showUnivUsing
  ) where

import Control.Exception (assert)
import Foreign
import Foreign.C
import System.IO.Unsafe


{#import Language.Lean.Internal.Exception #}
{#import Language.Lean.Internal.String #}
{#import Language.Lean.Name #}
{#import Language.Lean.Options #}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"

{#pointer lean_univ as UnivPtr -> Univ#}

{#fun unsafe lean_univ_mk_zero
  { id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_mk_succ
  { `UnivPtr'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_mk_max
  { `UnivPtr'
  , `UnivPtr'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_mk_imax
  { `UnivPtr'
  , `UnivPtr'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_mk_param
  { `NamePtr'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_mk_global
  { `NamePtr'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}


{#fun unsafe lean_univ_mk_meta
  { `NamePtr'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_to_string
  { `UnivPtr'
  , id `Ptr CString'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_to_string_using
  { `UnivPtr'
  , `OptionsPtr'
  , id `Ptr CString'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

foreign import ccall "&lean_univ_del"
  lean_univ_del_ptr :: FunPtr (UnivPtr -> IO ())

{#enum lean_univ_kind as UnivKind { upcaseFirstLetter }
         deriving (Eq)#}

{#fun pure unsafe lean_univ_get_kind
  { id `UnivPtr'
  } -> `UnivKind' #}

{#fun unsafe lean_univ_eq
  { `UnivPtr'
  , `UnivPtr'
  , id `Ptr CInt'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_geq
  { `UnivPtr'
  , `UnivPtr'
  , id `Ptr CInt'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_get_pred
  { `UnivPtr'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_get_max_lhs
  { `UnivPtr'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_get_max_rhs
  { `UnivPtr'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_get_name
  { `UnivPtr'
  , id `Ptr NamePtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_normalize
  { `UnivPtr'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_instantiate
  { `UnivPtr'
  , `CUInt'
  , id `Ptr NamePtr'
  , id `Ptr UnivPtr'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}


-- | A lean universe
newtype Univ = Univ (ForeignPtr Univ)

-- | Call a C layer function that attempts to allocate a
-- new universe
tryAllocUniv :: (Ptr UnivPtr -> Ptr ExceptionPtr -> IO Bool)
           -> IO Univ
tryAllocUniv mk_name =
  fmap Univ $ tryAllocLeanValue lean_univ_del_ptr $ mk_name

-- | Run an action with the underlying universe pointer.
withUnivPtr :: Univ -> (UnivPtr -> IO a) -> IO a
withUnivPtr (Univ nm) = withForeignPtr nm


data UnivView
   = UnivZero
     -- ^ The zero universe.
   | UnivSucc Univ
     -- ^ Successor of the previous universe.
   | UnivMax Univ Univ
     -- ^ Maximum of two universes.
   | UnivIMax Univ Univ
     -- ^ @UnivIMax x y@ denotes @y@ if @y@ is universe zero, otherwise @UnivMax x y@
   | UnivParam Name
     -- ^ Universe parameter with the given name.
   | UnivGlobal Name
     -- ^ Reference to a global universe.
   | UnivMeta Name
     -- ^ Meta variable with the given name.
  deriving (Show)

viewUniv :: Univ -> UnivView
viewUniv x = unsafePerformIO $ do
  withUnivPtr x $ \x_ptr -> do
  case (lean_univ_get_kind x_ptr) of
   LEAN_UNIV_ZERO -> return UnivZero
{-# NOINLINE viewUniv #-}

eqUniv :: Univ -> Univ -> Bool
eqUniv x y = unsafePerformIO $ do
  withUnivPtr x $ \x_ptr -> do
    withUnivPtr y $ \y_ptr -> do
      tryGetBool $ lean_univ_eq x_ptr y_ptr
{-# NOINLINE eqUniv #-}

geqUniv :: Univ -> Univ -> Bool
geqUniv x y = unsafePerformIO $ do
  withUnivPtr x $ \x_ptr -> do
    withUnivPtr y $ \y_ptr -> do
      tryGetBool $ lean_univ_geq x_ptr y_ptr
{-# NOINLINE geqUniv #-}

instance Eq Univ where
  (==) = eqUniv

showUniv :: Univ -> String
showUniv u = unsafePerformIO $ do
  withUnivPtr u $ \u_ptr ->
    tryAllocString $ lean_univ_to_string u_ptr
{-# NOINLINE showUniv #-}

showUnivUsing :: Univ -> Options -> String
showUnivUsing u options = unsafePerformIO $ do
  withUnivPtr u $ \u_ptr ->
    withOptionsPtr options $ \opt_ptr ->
      tryAllocString $ lean_univ_to_string_using u_ptr opt_ptr
{-# NOINLINE showUnivUsing #-}

instance Show Univ where
  show = showUniv

