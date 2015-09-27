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
  ( getEnum
  , getLeanValue
  , getLeanMaybeValue
  , tryGetLeanValue
  ) where

import Control.Lens (_Right)
import Foreign (Ptr)
import Foreign.C (CInt)
import System.IO.Unsafe (unsafePerformIO)
import Language.Lean.Internal.Exception

-- | Run a Lean function that returns an enum type or generates an
-- exception.
--
-- Other than allocating a new value or throwing an exception,
-- the function should be pure.
getEnum :: Enum a => LeanFn CInt -> a
getEnum alloc_fn =
  toEnum $ fromIntegral $ unsafePerformIO $ runLeanFn alloc_fn

-- | Run a Lean partial function that returns a Lean value.
--
-- Other than allocating a new value or exception, the function should be be
-- pure.
getLeanValue :: IsLeanValue a p => LeanFn p -> a
getLeanValue alloc_fn = unsafePerformIO $ do
  allocLeanValue alloc_fn
{-# INLINE getLeanValue #-}

-- | Try to run a Lean partial function that returns a Lean value
-- that will need to be freed.
--
-- Other than allocating a new value or exception, the function should be be
-- pure.
tryGetLeanValue :: IsLeanValue a p
                => (Ptr LeanException -> IO LeanException)
                -> LeanFn p
                -> Either LeanException a
tryGetLeanValue except_fn alloc_fn = unsafePerformIO $ do
  _Right mkLeanValue =<< tryRunLeanFn except_fn alloc_fn
{-# INLINE tryGetLeanValue #-}

-- | Try to run a Lean function that may return a Lean value
-- that will need to be freed.
--
-- Other than allocating a new value or throwing an exception,
-- the function should be pure.
getLeanMaybeValue :: IsLeanValue a p => LeanFn p -> Maybe a
getLeanMaybeValue alloc_fn = unsafePerformIO $ do
  traverse mkLeanValue =<< runLeanMaybeFn alloc_fn
{-# INLINE getLeanMaybeValue #-}
