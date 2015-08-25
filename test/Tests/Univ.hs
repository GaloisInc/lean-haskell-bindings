{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Univ (univTests) where

import Control.Lens
import Control.Monad

import Test.Tasty
import Test.Tasty.HUnit

import qualified Language.Lean as Lean

univTests :: TestTree
univTests = testGroup "Univ"
  [ testCase "zero"      testZero
  , testCase "one"       testOne
  , testCase "global"    testGlobal
  , testCase "max"       testMax
  , testCase "imax"      testIMax
  , testCase "normalize" testNormalize
  , testCase "instantiate" testInstantiate
  , testCase "showUniv"    testShowUnivUsing
  , testCase "geq"         testGeq
  ]

testZero :: Assertion
testZero = assert $ Lean.viewUniv 0 == Lean.UnivZero

testOne :: Assertion
testOne = assert $ Lean.viewUniv 1 == Lean.UnivSucc 0

testGlobal :: Assertion
testGlobal = assert $ Lean.viewUniv (Lean.globalUniv "U") == Lean.UnivGlobal "U"

p1, u :: Lean.Univ

-- p1 is "l_1"
p1 = Lean.paramUniv "l_1"
-- u is succ (max l_1 1)
u = Lean.succUniv (Lean.maxUniv (Lean.paramUniv "l_1") 1)

testMax :: Assertion
testMax = assert $ Lean.viewUniv (Lean.maxUniv p1 1) == Lean.UnivMax p1 1


testIMax :: Assertion
testIMax = assert $ Lean.viewUniv (Lean.imaxUniv 1 p1) == Lean.UnivIMax 1 p1

testNormalize :: Assertion
testNormalize = assert $ Lean.viewUniv x == y
  where x  = Lean.normalizeUniv u
        y  = Lean.UnivMax 2 (Lean.succUniv p1)

testInstantiate :: Assertion
testInstantiate = assert $ i == 2
  where i = Lean.instantiateUniv u [("l_1", 1)]

no_pp_unicode :: Lean.Options
no_pp_unicode = Lean.emptyOptions
              & Lean.boolOption "pp.unicode" .~ False

testShowUnivUsing :: Assertion
testShowUnivUsing = assert $ out == expected
  where out = Lean.showUnivUsing u no_pp_unicode
        expected = "(max l_1 1)+1"

testGeq :: Assertion
testGeq = assert $ 1 `Lean.geqUniv` 0
