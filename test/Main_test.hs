module Main (main) where

import Control.Exception
import Language.Lean.FFI

main = do
  bracket_ lean_initialize lean_finalize $ do
    bracket (lean_new_environment 0 True True True) lean_free_environment $ \_ -> do
      putStrLn "Lean (empty) environment was successfully created."
