{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Language.Lean as Lean
import Data.Monoid
import Control.Exception

main :: IO ()
main = do
  putStrLn "Started name test"
  let a = Lean.anonymousName
  case Lean.viewName a of
    Lean.AnonymousName -> return ()
    _ -> fail "Expected anonymous name."

  let n2 = "foo.bla"

  putStrLn $ "Lean name: " ++ show n2

  case Lean.viewName n2 of
    Lean.StringName{} -> return ()
    _ -> fail "Expected string name"

  let n3 = n2 `Lean.idxName` 1
  putStrLn $ "Lean name: " ++ show n3

  case Lean.viewName n3 of
    Lean.IndexName n4 1 | n4 == n2 -> do
      return ()
    _ -> fail $ "Expected index name"

  -- TODO: Test exception generation on partial function

{-
    check(!lean_get_name_prefix(a, &n5, &ex));
    s3 = lean_get_exception_message(ex);
    printf("Lean exception: %s\n", s3);
-}

  let badName = "foo.1dog"
  res <- try $ (return $! badName) :: IO (Either Lean.LeanException Lean.Name)
  case res of
    Left _ -> return ()
    Right _ -> fail "Expected failure while building Lean name."
