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
  , geqUniv
  , showUniv
  , showUnivUsing
  , normalizeUniv
  , instantiateUniv
  ) where

import Control.Exception (throw)
import Control.Monad (when)
import Foreign
import Foreign.C
import System.IO.Unsafe

{#import Language.Lean.Internal.Exception #}
{#import Language.Lean.Internal.Name #}
{#import Language.Lean.Internal.Options #}

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
{#pointer lean_univ as Univ foreign newtype#}

{#pointer lean_univ as UnivPtr -> Univ#}

foreign import ccall "&lean_univ_del"
  lean_univ_del_ptr :: FunPtr (UnivPtr -> IO ())

-- | Call a C layer function that attempts to allocate a new universe and is pure.
tryAllocUniv :: LeanPartialFn UnivPtr -> Univ
tryAllocUniv = Univ . tryAllocLeanValue lean_univ_del_ptr

{#fun unsafe lean_univ_mk_zero
  { id `Ptr UnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_mk_succ
  { `Univ'
  , id `Ptr UnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_mk_max
  { `Univ'
  , `Univ'
  , id `Ptr UnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_mk_imax
  { `Univ'
  , `Univ'
  , id `Ptr UnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_mk_param
  { `Name'
  , id `Ptr UnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_mk_global
  { `Name'
  , id `Ptr UnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}


{#fun unsafe lean_univ_mk_meta
  { `Name'
  , id `Ptr UnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_to_string
  { `Univ'
  , id `Ptr CString'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_to_string_using
  { `Univ'
  , `Options'
  , id `Ptr CString'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#enum lean_univ_kind as UnivKind { upcaseFirstLetter }
         deriving (Eq)#}

{#fun pure unsafe lean_univ_get_kind
  { `Univ'
  } -> `UnivKind' #}

{#fun unsafe lean_univ_eq
  { `Univ'
  , `Univ'
  , id `Ptr CInt'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_geq
  { `Univ'
  , `Univ'
  , id `Ptr CInt'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_get_pred
  { `Univ'
  , id `Ptr UnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_get_max_lhs
  { `Univ'
  , id `Ptr UnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_get_max_rhs
  { `Univ'
  , id `Ptr UnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_get_name
  { `Univ'
  , id `Ptr NamePtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_normalize
  { `Univ'
  , id `Ptr UnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_univ_instantiate
  { `Univ'
  , `Word32'
  , id `Ptr NamePtr'
  , id `Ptr UnivPtr'
  , id `Ptr UnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

eqUniv :: Univ -> Univ -> Bool
eqUniv x y = tryGetBool $ lean_univ_eq x y

geqUniv :: Univ -> Univ -> Bool
geqUniv x y = tryGetBool $ lean_univ_geq x y

instance Eq Univ where
  (==) = eqUniv

-- | Shwo a universe.
showUniv :: Univ -> String
showUniv u = tryAllocString $ lean_univ_to_string u

-- | Show a universe with the given options.
showUnivUsing :: Univ -> Options -> String
showUnivUsing u options = tryAllocString $ lean_univ_to_string_using u options

instance Show Univ where
  show = showUniv

-- | A view of a universe.
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
  withForeignArray withName sz (fst <$> args) $ \name_array -> do
    withForeignArray withUniv sz (snd <$> args) $ \univ_array -> do
       lean_univ_instantiate x (fromIntegral sz) name_array univ_array r_ptr e_ptr
