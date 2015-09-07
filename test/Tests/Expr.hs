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

u0 :: Lean.Univ
u0 = Lean.explicitUniv 0

testVar :: IO ()
testVar = do
  let e1 = Lean.varExpr 0
  let e2 = Lean.varExpr 1
  let e3 = Lean.varExpr 0
  assert (e1 == e3)
  assert (not (e1 == e2))
  assert (Lean.exprView e1 == Lean.ExprVar 0)
  assert (Lean.exprView e2 == Lean.ExprVar 1)

testConst :: IO ()
testConst = do
  let e1 = Lean.constExpr "id"   [u0]
  let e2 = Lean.constExpr "id"   [u0, u0]
  let e3 = Lean.constExpr "func" [u0, u0]
  assert $ e1 == e1
  assert $ not (e1 == e2)
  assert $ Lean.exprView e2 == Lean.ExprConst "id" [u0, u0]
  assert $ Lean.exprView e3 == Lean.ExprConst "func" [u0, u0]
  assert $ Lean.exprToString e3 == "func.{0 0}"
