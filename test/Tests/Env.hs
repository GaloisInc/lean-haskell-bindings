{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Env (envTests) where

import Control.Exception (catch)
import Control.Lens
import Test.Tasty
import Test.Tasty.HUnit

import Language.Lean as Lean

envTests :: TestTree
envTests = testGroup "Env"
  [ testCase "add_univ" testAddUniv
  , testCase "id"       testId
  , testCase "import"   testImport
  ]

u0 :: Univ
u0 = explicitUniv 0

u1 :: Univ
u1 = explicitUniv 1

testAddUniv :: IO ()
testAddUniv = do
  let env = standardEnv trustHigh
  let new_env = env & envAddUniv "u"
  assert (new_env      `envContainsUniv` "u")
  assert (not (new_env `envContainsUniv` "v"))
  assert (new_env^..envUnivs == ["u"])
  -- Check that adding duplicate names throws an exception.
  let bad_env = new_env & envAddUniv "u"
  catch (seq bad_env $ assertFailure "Expected exception from duplicate universe addition")
        (\e -> assert (exceptionKind e == LeanKernelException))

testId :: IO ()
testId = do
  let env = standardEnv trustHigh
  let v0 = varExpr 0
  let v1 = varExpr 1
  let tp = sortExpr (paramUniv "l")
  let id_type = piExpr BinderDefault "A" tp
              $ piExpr BinderDefault "a" v0 v1
  let f      = lambdaExpr BinderDefault "a" v0 v0
  let id_val = lambdaExpr BinderDefault "A" tp f
  let id_def = definition "id" ["l"] id_type id_val 0 True
  assert $ f `exprLt` id_val
  assert $ id_val `exprLt` f == False

  assert $ exprToString id_type == "Pi (A : Type.{l}) (a : A), A"
  assert $ exprToString id_val  == "fun (A : Type.{l}) (a : A), a"

  let id_cert_def = check env id_def
  let new_env = env & envAddDecl id_cert_def

  assert $     env `envContainsDecl` "id" == False
  assert $ new_env `envContainsDecl` "id"

  let decls = fmap declView $ new_env^..envDecls

  assert $ length decls == 1

  let prop = sortExpr u0

  let id1 = constExpr "id" [u1]
  let id1T1 = id1 `appExpr` prop
  let id1T1T0 = id1T1 `appExpr` sortExpr u1

  let tc = typechecker new_env
  let (n1, _s1) = whnf tc id1T1T0
  assert $ n1 == sortExpr u1

  let (n2, _s2) = inferType tc id1T1
  let (r,_cs) = isDefEq tc n2 (piExpr BinderDefault "a" prop prop)
  assert r

testImport :: IO ()
testImport = do
  let env = standardEnv trustHigh
  ios <- mkBufferedIOState
  new_env <- envImport ios env ["init.logic"]
  assert $ envContainsDecl new_env "not"

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
