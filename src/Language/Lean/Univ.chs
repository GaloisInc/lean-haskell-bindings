{-|
Module      : Language.Lean.Univ
Description : Operations on Lean Universes
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com
Stability   : experimental
Portability : POSIX

This module defines operations for universe levels.
-}
{- LANGUAGE CPP #-}
{- LANGUAGE DoAndIfThenElse #-}
{- LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{- LANGUAGE GeneralizedNewtypeDeriving #-}
{- LANGUAGE MultiParamTypeClasses #-}
{- LANGUAGE StandaloneDeriving #-}
{- LANGUAGE TypeFamilies #-}
module Language.Lean.Univ
  ( Univ
  , zeroUniv
  , succUniv
  , maxUniv
  , imaxUniv
  , paramUniv
  , globalUniv
  , metaUniv
  , UnivView(..)
  , viewUniv
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
import GHC.Exts (fromList)
import System.IO.Unsafe

import Language.Lean.List
{#import Language.Lean.Internal.Exception #}
{#import Language.Lean.Internal.Name #}
{#import Language.Lean.Internal.Univ#}

#include "lean_macros.h"
#include "lean_bool.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"

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
viewUniv :: Univ -> UnivView
viewUniv x =
  case lean_univ_get_kind x of
   LEAN_UNIV_ZERO -> UnivZero
   LEAN_UNIV_SUCC -> UnivSucc (tryGetLeanValue $ lean_univ_get_pred x)
   LEAN_UNIV_MAX ->
     UnivMax (tryGetLeanValue $ lean_univ_get_max_lhs x)
             (tryGetLeanValue $ lean_univ_get_max_rhs x)
   LEAN_UNIV_IMAX ->
     UnivIMax (tryGetLeanValue $ lean_univ_get_max_lhs x)
              (tryGetLeanValue $ lean_univ_get_max_rhs x)
   LEAN_UNIV_PARAM  -> UnivParam  (tryGetLeanValue $ lean_univ_get_name x)
   LEAN_UNIV_GLOBAL -> UnivGlobal (tryGetLeanValue $ lean_univ_get_name x)
   LEAN_UNIV_META   -> UnivMeta   (tryGetLeanValue $ lean_univ_get_name x)

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
univGeq x y = tryGetLeanValue $ lean_univ_geq x y

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
normalizeUniv x = tryGetLeanValue $ lean_univ_normalize x

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
instantiateUniv2 u nms args = tryGetLeanValue $ lean_univ_instantiate u nms args

{#fun unsafe lean_univ_instantiate
  { `Univ'
  , `ListName'
  , `ListUniv'
  , `OutUnivPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}
