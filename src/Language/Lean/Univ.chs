{-|
Module      : Language.Lean.Univ
Description : Operations on Lean Universes
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com
Stability   : experimental
Portability : POSIX

This module defines functions for constructing and deconstructing lean universes.
-}
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

import Control.Exception (assert, throw)
import Control.Monad (when)
import Foreign
import Foreign.C
import System.IO.Unsafe


{#import Language.Lean.Internal.Exception #}
{#import Language.Lean.Internal.Name #}
{#import Language.Lean.Internal.Options #}
{#import Language.Lean.Internal.String #}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"

type WithPointer b p a = b -> (p -> IO a) -> IO a

withForeignArray :: Storable p => WithPointer b p a -> Int -> [b] -> (Ptr p -> IO a) -> IO a
withForeignArray withPtr n l action = do
  allocaArray n $ \ptr -> do
    withForeignArray' withPtr ptr l action

withForeignArray' :: Storable p => WithPointer b p a -> Ptr p -> [b] -> (Ptr p -> IO a) -> IO a
withForeignArray' _ arrayPtr [] action = action arrayPtr
withForeignArray' withPtr arrayPtr (h:r) action = do
  withPtr h $ \ptr -> do
    poke arrayPtr ptr
    withForeignArray' withPtr (arrayPtr `plusPtr` sizeOf ptr) r action


-- | A lean universe
newtype Univ = Univ (ForeignPtr Univ)

{#pointer lean_univ as UnivPtr -> Univ#}

foreign import ccall "&lean_univ_del"
  lean_univ_del_ptr :: FunPtr (UnivPtr -> IO ())

-- | Call a C layer function that attempts to allocate a new universe and is pure.
tryAllocUniv :: LeanPartialFn UnivPtr
             -> Univ
tryAllocUniv mk_name = unsafePerformIO $ tryAllocUnivIO mk_name

-- | Call a C layer function that attempts to allocate a new universe.
tryAllocUnivIO :: LeanPartialFn UnivPtr
               -> IO Univ
tryAllocUnivIO mk_name = fmap Univ $ tryAllocLeanValue lean_univ_del_ptr $ mk_name

-- | Run an action with the underlying universe pointer.
withUnivPtr :: Univ -> (UnivPtr -> IO a) -> IO a
withUnivPtr (Univ nm) = withForeignPtr nm

{#fun unsafe lean_univ_mk_zero
  { id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_mk_succ
  { withUnivPtr* `Univ'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_mk_max
  { withUnivPtr* `Univ'
  , withUnivPtr* `Univ'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_mk_imax
  { withUnivPtr* `Univ'
  , withUnivPtr* `Univ'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_mk_param
  { withNamePtr* `Name'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_mk_global
  { withNamePtr* `Name'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}


{#fun unsafe lean_univ_mk_meta
  { withNamePtr* `Name'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_to_string
  { withUnivPtr* `Univ'
  , id `Ptr CString'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_to_string_using
  { withUnivPtr* `Univ'
  , withOptionsPtr* `Options'
  , id `Ptr CString'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#enum lean_univ_kind as UnivKind { upcaseFirstLetter }
         deriving (Eq)#}

{#fun pure unsafe lean_univ_get_kind
  { withUnivPtr* `Univ'
  } -> `UnivKind' #}

{#fun unsafe lean_univ_eq
  { withUnivPtr* `Univ'
  , withUnivPtr* `Univ'
  , id `Ptr CInt'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_geq
  { withUnivPtr* `Univ'
  , withUnivPtr* `Univ'
  , id `Ptr CInt'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_get_pred
  { withUnivPtr* `Univ'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_get_max_lhs
  { withUnivPtr* `Univ'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_get_max_rhs
  { withUnivPtr* `Univ'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_get_name
  { withUnivPtr* `Univ'
  , id `Ptr NamePtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_normalize
  { withUnivPtr* `Univ'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_instantiate
  { withUnivPtr* `Univ'
  , `Word32'
  , id `Ptr NamePtr'
  , id `Ptr UnivPtr'
  , id `Ptr UnivPtr'
  , id `Ptr ExceptionPtr'
  } -> `Bool' #}

eqUniv :: Univ -> Univ -> Bool
eqUniv x y = unsafePerformIO $ tryGetBool $ lean_univ_eq x y

geqUniv :: Univ -> Univ -> Bool
geqUniv x y = unsafePerformIO $ tryGetBool $ lean_univ_geq x y

instance Eq Univ where
  (==) = eqUniv

showUniv :: Univ -> String
showUniv u = unsafePerformIO $ tryAllocString $ lean_univ_to_string u

showUnivUsing :: Univ -> Options -> String
showUnivUsing u options = unsafePerformIO $ do
  tryAllocString $ lean_univ_to_string_using u options

instance Show Univ where
  show = showUniv

data UnivView
   = UnivZero
     -- ^ The zero universe.
   | UnivSucc !Univ
     -- ^ Successor of the previous universe.
   | UnivMax !Univ !Univ
     -- ^ Maximum of two universes.
   | UnivIMax !Univ !Univ
     -- ^ @UnivIMax x y@ denotes @y@ if @y@ is universe zero, otherwise @UnivMax x y@
   | UnivParam !Name
     -- ^ Universe parameter with the given name.
   | UnivGlobal !Name
     -- ^ Reference to a global universe.
   | UnivMeta !Name
     -- ^ Meta variable with the given name.
  deriving (Show)

viewUniv :: Univ -> UnivView
viewUniv x =
  case lean_univ_get_kind x of
   LEAN_UNIV_ZERO -> UnivZero
   LEAN_UNIV_SUCC -> UnivSucc (tryAllocUniv $ lean_univ_get_pred x)
   LEAN_UNIV_MAX ->
     UnivMax (tryAllocUniv $ lean_univ_get_max_lhs x)
             (tryAllocUniv $ lean_univ_get_max_rhs x)
   LEAN_UNIV_IMAX ->
     UnivIMax (tryAllocUniv $ lean_univ_get_max_lhs x)
              (tryAllocUniv $ lean_univ_get_max_rhs x)
   LEAN_UNIV_PARAM  -> UnivParam  (tryAllocName $ lean_univ_get_name x)
   LEAN_UNIV_GLOBAL -> UnivGlobal (tryAllocName $ lean_univ_get_name x)
   LEAN_UNIV_META   -> UnivMeta   (tryAllocName $ lean_univ_get_name x)

-- | Return the normal form for a universe.
normalizeUniv :: Univ -> Univ
normalizeUniv x = tryAllocUniv $ lean_univ_normalize x

-- | Instantiate the parameters with universes
instantiateUniv :: Univ -> [(Name, Univ)] -> Univ
instantiateUniv x args = tryAllocUniv $ \r_ptr e_ptr -> do
  let sz = length args
  when (sz > fromIntegral (maxBound :: Word32)) $ do
    throw $ leanKernelException "Lean does not support instantiating more than 2^32 paramters universe parameters."
  withForeignArray withNamePtr sz (fst <$> args) $ \name_array -> do
    withForeignArray withUnivPtr sz (snd <$> args) $ \univ_array -> do
       lean_univ_instantiate x (fromIntegral sz) name_array univ_array r_ptr e_ptr
