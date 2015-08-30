{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Expr (exprTests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Language.Lean as Lean

exprTests :: TestTree
exprTests = testGroup "Expr"
  [ testCase "var"      testVar
  , testCase "const"    testConst
  ]

testVar :: IO ()
testVar = do
  let e1 = Lean.varExpr 0
  let e2 = Lean.varExpr 1
  let e3 = Lean.varExpr 0
  assert (e1 == e3)
  assert (not (e1 == e2))
  assert (Lean.viewExpr e1 == Lean.ExprVar 0)
  assert (Lean.viewExpr e2 == Lean.ExprVar 1)

testConst :: IO ()
testConst = do
  let e1 = Lean.constExpr "id"   [0]
  let e2 = Lean.constExpr "id"   [0,0]
  let e3 = Lean.constExpr "func" [0,0]
  assert (e1 == e1)
  assert (not (e1 == e2))
  assert (Lean.viewExpr e2 == Lean.ExprConst "id" [0, 0])
  assert (Lean.viewExpr e3 == Lean.ExprConst "func" [0, 0])
  assert (show e3 == "func.{0 0}")
