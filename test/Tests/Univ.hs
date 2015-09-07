{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Univ (univTests) where

import Control.Lens

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
  , testCase "lt"        testUnivLt
  , testCase "instantiate" testInstantiate
  , testCase "showUniv"    testShowUnivUsing
  , testCase "geq"         testGeq
  ]

testZero :: Assertion
testZero = assert $ Lean.univView u0 == Lean.UnivZero

testOne :: Assertion
testOne = assert $ Lean.univView u1 == Lean.UnivSucc u0

testGlobal :: Assertion
testGlobal = assert $ Lean.univView (Lean.globalUniv "U") == Lean.UnivGlobal "U"

testMax :: Assertion
testMax = assert $ Lean.univView max_p1_1 == Lean.UnivMax p1 u1
  where max_p1_1 = Lean.maxUniv p1 (Lean.explicitUniv 1)

testIMax :: Assertion
testIMax = assert $ Lean.univView (Lean.imaxUniv u1 p1) == Lean.UnivIMax u1 p1

testUnivLt :: Assertion
testUnivLt = do
  assert $ u0 `Lean.univLt` u1
  assert $ not (u1 `Lean.univLt` u0)

testNormalize :: Assertion
testNormalize = assert $ Lean.univView x == y
  where x  = Lean.normalizeUniv u
        y  = Lean.UnivMax u2 (Lean.succUniv p1)


testInstantiate :: Assertion
testInstantiate = assert $ i == u2
  where i = Lean.instantiateUniv u [("l_1", u1)]

no_pp_unicode :: Lean.Options
no_pp_unicode = Lean.emptyOptions
              & Lean.boolOption "pp.unicode" .~ False

testShowUnivUsing :: Assertion
testShowUnivUsing = assert $ out == expected
  where out = Lean.showUnivUsing u no_pp_unicode
        expected = "(max l_1 1)+1"

testGeq :: Assertion
testGeq = assert $ u1 `Lean.univGeq` u0

------------------------------------------------------------------------
-- Universe expressions used in test cases

p1, u :: Lean.Univ

-- p1 is "l_1"
p1 = Lean.paramUniv "l_1"
-- u is succ (max l_1 1)
u = Lean.succUniv (Lean.maxUniv (Lean.paramUniv "l_1") u1)

u0 :: Lean.Univ
u0 = Lean.explicitUniv 0

u1 :: Lean.Univ
u1 = Lean.explicitUniv 1

u2 :: Lean.Univ
u2 = Lean.explicitUniv 2
