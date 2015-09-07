{-# LANGUAGE OverloadedStrings #-}
module Tests.Name (nameTests) where

import Control.Exception (try)
import Control.Monad

import Test.Tasty
import Test.Tasty.HUnit

import qualified Language.Lean as Lean

nameTests :: TestTree
nameTests = testGroup "Name"
  [ testCase "anonymous" anonymousTest
  , testCase "string" stringTest
  , testCase "idx" idxTest
  , testCase "bad_name" badNameTest
  ]

anonymousTest :: IO ()
anonymousTest = do
  case Lean.nameView Lean.anonymousName of
    Lean.AnonymousName -> return ()
    _ -> fail "Expected anonymous name."

stringTest :: IO ()
stringTest = do
  case Lean.nameView "foo.bla" of
    Lean.StringName "foo" "bla" -> return ()
    _ -> fail "Expected string name"

  when (show ("foo.bla" :: Lean.Name) /= "foo.bla") $ do
    fail "Name did not match expected."

idxTest :: IO ()
idxTest = do
  case Lean.nameView "foo.bla.1" of
    Lean.IndexName "foo.bla" 1 -> do
      return ()
    _ -> fail $ "Expected index name"

badNameTest :: IO ()
badNameTest = do
  res <- try $ (return $! "foo.1dog") :: IO (Either Lean.LeanException Lean.Name)
  case res of
    Left _ -> return ()
    Right _ -> fail "Expected failure while building Lean name."
