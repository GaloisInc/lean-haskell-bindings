{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Env (envTests) where

import Control.Lens
import Test.Tasty
import Test.Tasty.HUnit

import qualified Language.Lean as Lean

envTests :: TestTree
envTests = testGroup "Env"
  [ testCase "add_univ" testAddUniv
  , testCase "id"       testId
  , testCase "import"   testImport
  ]

u0 :: Lean.Univ
u0 = Lean.explicitUniv 0

u1 :: Lean.Univ
u1 = Lean.explicitUniv 1

testAddUniv :: IO ()
testAddUniv = do
  let env = Lean.standardEnv Lean.trustHigh
  let new_env = env & Lean.envAddUniv "u"
  assert (new_env      `Lean.envContainsUniv` "u")
  assert (not (new_env `Lean.envContainsUniv` "v"))
  assert (new_env^..Lean.envUnivs == ["u"])

testId :: IO ()
testId = do
  let env = Lean.standardEnv Lean.trustHigh
  let v0 = Lean.varExpr 0
  let v1 = Lean.varExpr 1
  let tp = Lean.sortExpr (Lean.paramUniv "l")
  let id_type = Lean.piExpr Lean.BinderDefault "A" tp
              $ Lean.piExpr Lean.BinderDefault "a" v0 v1
  let f      = Lean.lambdaExpr Lean.BinderDefault "a" v0 v0
  let id_val = Lean.lambdaExpr Lean.BinderDefault "A" tp f
  let id_def = Lean.definition "id" ["l"] id_type id_val 0 True
  assert $ f `Lean.exprLt` id_val
  assert $ id_val `Lean.exprLt` f == False


  assert $ show id_type ==  "Pi (A : Type.{l}) (a : A), A"
  assert $ show id_val  ==  "fun (A : Type.{l}) (a : A), a"

  let id_cert_def = Lean.check env id_def
  let new_env = env & Lean.envAddDecl id_cert_def

  assert $ env `Lean.envContainsDecl` "id" == False
  assert $ new_env `Lean.envContainsDecl` "id"

  let decls = fmap Lean.declView $ new_env^..Lean.envDecls

  assert $ length decls == 1

  let prop = Lean.sortExpr u0

  let id1 = Lean.constExpr "id" [u1]
  let id1T1 = id1 `Lean.appExpr` prop
  let id1T1T0 = id1T1 `Lean.appExpr` Lean.sortExpr u1

  let tc = Lean.typechecker new_env
  let (n1, _s1) = Lean.whnf tc id1T1T0
  assert $ n1 == Lean.sortExpr u1

  let (n2, _s2) = Lean.inferType tc id1T1
  let (r,_cs) = Lean.isDefEq tc n2 (Lean.piExpr Lean.BinderDefault "a" prop prop)
  assert r

testImport :: IO ()
testImport = do
  let env = Lean.standardEnv Lean.trustHigh
  ios <- Lean.mkBufferedIOState
  new_env <- Lean.envImport ios env ["init.logic"]
  assert $ Lean.envContainsDecl new_env "not"

{-
testImportPar :: IO ()
testImportPar = do
  v0 <- newEmptyMVar
  v1 <- newEmptyMVar
  v2 <- newEmptyMVar
  v3 <- newEmptyMVar
  v4 <- newEmptyMVar
  v5 <- newEmptyMVar
  v6 <- newEmptyMVar
  v7 <- newEmptyMVar
  v8 <- newEmptyMVar
  v9 <- newEmptyMVar
  _ <- forkOS $ testImport >> putMVar v0 True
  _ <- forkOS $ testImport >> putMVar v1 True
  _ <- forkOS $ testImport >> putMVar v2 True
  _ <- forkOS $ testImport >> putMVar v3 True
  _ <- forkOS $ testImport >> putMVar v4 True
  _ <- forkOS $ testImport >> putMVar v5 True
  _ <- forkOS $ testImport >> putMVar v6 True
  _ <- forkOS $ testImport >> putMVar v7 True
  _ <- forkOS $ testImport >> putMVar v8 True
  _ <- forkOS $ testImport >> putMVar v9 True
  True <- takeMVar v0
  True <- takeMVar v1
  True <- takeMVar v2
  True <- takeMVar v3
  True <- takeMVar v4
  True <- takeMVar v5
  True <- takeMVar v6
  True <- takeMVar v7
  True <- takeMVar v8
  True <- takeMVar v9
  return ()
-}
