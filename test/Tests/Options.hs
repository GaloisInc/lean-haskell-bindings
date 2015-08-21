{-# LANGUAGE OverloadedStrings #-}
module Tests.Options (optionsTests) where

import Control.Exception (try)
import Control.Lens
import Control.Monad
import System.IO

import Test.Tasty
import Test.Tasty.HUnit

import qualified Language.Lean as Lean

optionsTests :: TestTree
optionsTests = testCase "Options" runOptionTest

runOptionTest :: IO ()
runOptionTest = do
  let o1 = Lean.emptyOptions
  let o2 = o1 & Lean.boolOption "pp.unicode" .~ True
  let o4 = o2 & Lean.uintOption "verbose" .~ 10
              & Lean.doubleOption   "seed" .~ 1.23
  let o5 = o1 & Lean.boolOption "pp.unicode" .~ True
  unless (o2 == o5) $ do
    fail $ "Expected options o2 and o5 to be equivalent."

  unless (show o4 == "⟨seed ↦ 1.23, verbose ↦ 10, pp.unicode ↦ true⟩") $ do
    fail $ "Lean o4 options does not render as expected."

  unless (o4^.Lean.boolOption "pp.unicode" == True) $ do
    fail $ "Expected pp.unicode in o4 to be True."


  unless (o4^.Lean.uintOption "verbose" == 10) $ do
    fail $ "Expected verbose in o4 to be True."

  let o6 = o4 & Lean.stringOption "output" .~ "~/tmp/file.olean"
  unless (o6^.Lean.stringOption "output" ==  "~/tmp/file.olean") $ do
    fail $ "Expected output in o6 to be ~/tmp/file.olean."
