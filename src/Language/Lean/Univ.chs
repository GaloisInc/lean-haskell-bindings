{-|
Module      : Language.Lean.Univ
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Operations for universe levels.
-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Trustworthy #-}
module Language.Lean.Univ
  ( Univ
  , zeroUniv
  , succUniv
  , maxUniv
  , imaxUniv
  , paramUniv
  , globalUniv
  , metaUniv
  , explicitUniv
  , UnivView(..)
  , univView
  , showUniv
  , showUnivUsing
    -- * Operations on universe levels
  , normalizeUniv
  , instantiateUniv
  , instantiateUniv2
  , univGeq
  , univLt
  ) where

import Foreign
import Foreign.C
import System.IO.Unsafe

import Language.Lean.List
{#import Language.Lean.Internal.Exception #}
import Language.Lean.Internal.Exception.Unsafe
{#import Language.Lean.Internal.Name #}
{#import Language.Lean.Internal.Univ#}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"

------------------------------------------------------------------------
-- Operations for constructing universes

-- | The zero universe
zeroUniv :: Univ
zeroUniv = getLeanValue $ lean_univ_mk_zero

{#fun unsafe lean_univ_mk_zero
 { `OutUnivPtr', `OutExceptionPtr' } -> `Bool' #}

-- | Successor of the universe
succUniv :: Univ -> Univ
succUniv x = getLeanValue $ lean_univ_mk_succ x

{#fun unsafe lean_univ_mk_succ
 { `Univ', `OutUnivPtr', `OutExceptionPtr' } -> `Bool' #}

-- | The max of two universes.
maxUniv :: Univ -> Univ -> Univ
maxUniv x y = getLeanValue $ lean_univ_mk_max x y

{#fun unsafe lean_univ_mk_max
 { `Univ', `Univ', `OutUnivPtr', `OutExceptionPtr' } -> `Bool' #}

-- | The imax of two universes.
imaxUniv :: Univ -> Univ -> Univ
imaxUniv x y = getLeanValue $ lean_univ_mk_imax x y

{#fun unsafe lean_univ_mk_imax
 { `Univ', `Univ', `OutUnivPtr', `OutExceptionPtr' } -> `Bool' #}

-- | A universe parameter of the given name.
paramUniv :: Name -> Univ
paramUniv x = getLeanValue $ lean_univ_mk_param x

{#fun unsafe lean_univ_mk_param
 { `Name', `OutUnivPtr', `OutExceptionPtr' } -> `Bool' #}

-- | A global universe with the given name.
globalUniv :: Name -> Univ
globalUniv x = getLeanValue $ lean_univ_mk_global x

{#fun unsafe lean_univ_mk_global
 { `Name', `OutUnivPtr', `OutExceptionPtr' } -> `Bool' #}

-- | A universe meta-variable with the given name.
metaUniv :: Name -> Univ
metaUniv x = getLeanValue $ lean_univ_mk_meta x

{#fun unsafe lean_univ_mk_meta
 { `Name', `OutUnivPtr', `OutExceptionPtr' } -> `Bool' #}

-- | Create an explicit universe level
explicitUniv :: Integer -> Univ
explicitUniv i0 | i0 < 0 = error "Universes cannot be negative."
                | otherwise = go zeroUniv i0
  where -- Make sure first argument is evaluated
        go r _ | seq r False = error "unexpected"
        go r 0 = r
        go r i = go (succUniv r) (i-1)

------------------------------------------------------------------------
-- View

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
  deriving (Eq, Ord, Show)

-- | Create a view of the universe.
univView :: Univ -> UnivView
univView x =
  case lean_univ_get_kind x of
   LEAN_UNIV_ZERO -> UnivZero
   LEAN_UNIV_SUCC -> UnivSucc (getLeanValue $ lean_univ_get_pred x)
   LEAN_UNIV_MAX ->
     UnivMax (getLeanValue $ lean_univ_get_max_lhs x)
             (getLeanValue $ lean_univ_get_max_rhs x)
   LEAN_UNIV_IMAX ->
     UnivIMax (getLeanValue $ lean_univ_get_max_lhs x)
              (getLeanValue $ lean_univ_get_max_rhs x)
   LEAN_UNIV_PARAM  -> UnivParam  (getLeanValue $ lean_univ_get_name x)
   LEAN_UNIV_GLOBAL -> UnivGlobal (getLeanValue $ lean_univ_get_name x)
   LEAN_UNIV_META   -> UnivMeta   (getLeanValue $ lean_univ_get_name x)

{#enum lean_univ_kind as UnivKind { upcaseFirstLetter }
         deriving (Eq)#}

{#fun pure unsafe lean_univ_get_kind
 { `Univ' } -> `UnivKind' #}

{#fun unsafe lean_univ_get_pred
 { `Univ', `OutUnivPtr', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_univ_get_max_lhs
 { `Univ', `OutUnivPtr', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_univ_get_max_rhs
 { `Univ', `OutUnivPtr', `OutExceptionPtr' } -> `Bool' #}

{#fun unsafe lean_univ_get_name
 { `Univ', `OutNamePtr', `OutExceptionPtr' } -> `Bool' #}

-- | @geqUniv x y@ returns @true@ if @y@ is a larger universe level
-- than @x@ for all possible assignments to the variables in the
-- @x@ and @y@.
univGeq :: Univ -> Univ -> Bool
univGeq x y = getLeanValue $ lean_univ_geq x y

{#fun unsafe lean_univ_geq
  { `Univ'
  , `Univ'
  , id `Ptr CInt'
  , `OutExceptionPtr'
  } -> `Bool' #}


------------------------------------------------------------------------
-- Normalize

{#fun unsafe lean_univ_normalize
  { `Univ'
  , `OutUnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

-- | Return the normal form for a universe.
normalizeUniv :: Univ -> Univ
normalizeUniv x = getLeanValue $ lean_univ_normalize x

------------------------------------------------------------------------
-- Instantiate

-- | Instantiate the parameters with universes
instantiateUniv :: Univ -> [(Name,Univ)] -> Univ
instantiateUniv u bindings =
  instantiateUniv2 u (fromList (fst <$> bindings)) (fromList (snd <$> bindings))

-- | Instantiate the parameters with universes using separate lists for names and levels.
instantiateUniv2 :: Univ
                 -> List Name
                 -> List Univ
                 -> Univ
instantiateUniv2 u nms args = getLeanValue $ lean_univ_instantiate u nms args

{#fun unsafe lean_univ_instantiate
  { `Univ'
  , `ListName'
  , `ListUniv'
  , `OutUnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}
