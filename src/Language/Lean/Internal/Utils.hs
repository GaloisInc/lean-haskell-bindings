
module Language.Lean.Internal.Utils
  ( WithValueFn
  , BinaryPred
  , withBinaryPred
  ) where

import System.IO.Unsafe

-- |
type WithValueFn v p a = v -> (p -> IO a) -> IO a

-- | Binary predicate that takes two arguments
type BinaryPred a = a -> a -> Bool

-- | Lift binary predicate from a value of type @p@ to a binary
-- predicate over type @a@
withBinaryPred :: WithValueFn a p Bool -> BinaryPred p -> BinaryPred a
withBinaryPred withPtr binPred x y =  unsafePerformIO $ do
  withPtr x $ \x_ptr -> do
    withPtr y $ \y_ptr -> do
      return $! binPred x_ptr y_ptr
