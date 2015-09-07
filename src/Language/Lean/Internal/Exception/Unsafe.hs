{-|
Module      : Language.Lean.Internal.Exception.Unsafe
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Operations for getting Lean values from functions that must
not have side effects.
-}
{-# LANGUAGE Unsafe #-}
module Language.Lean.Internal.Exception.Unsafe
  ( tryGetEnum
  , tryGetLeanValue
  , tryGetLeanMaybeValue
  ) where

import Foreign.C (CInt)
import System.IO.Unsafe (unsafePerformIO)
import Language.Lean.Internal.Exception

-- | Try to run a Lean partial function that returns a Lean value
-- that will need to be freed.
--
-- Other than allocating a new value or exception, the function
-- should be be pure.
tryGetLeanValue :: IsLeanValue a p
                => LeanPartialFn p
                -> a
tryGetLeanValue alloc_fn = unsafePerformIO $ do
  mkLeanValue =<< runLeanPartialFn alloc_fn
{-# INLINE tryGetLeanValue #-}

-- | Try to run a Lean function that may return a Lean value
-- that will need to be freed.
--
-- Other than allocating a new value or throwing an exception,
-- the function should be pure.
tryGetLeanMaybeValue :: IsLeanValue a p
                     => LeanPartialFn p
                     -> Maybe a
tryGetLeanMaybeValue alloc_fn = unsafePerformIO $ do
  traverse mkLeanValue =<< runLeanMaybeFn alloc_fn
{-# INLINE tryGetLeanMaybeValue #-}

-- | Try to run a Lean partial function that returns an enum type
--
-- Other than allocating a new value or throwing an exception,
-- the function should be pure.
tryGetEnum :: (Enum a) => LeanPartialFn CInt -> a
tryGetEnum alloc_fn =
  toEnum $ fromIntegral $ unsafePerformIO $ runLeanPartialFn alloc_fn
