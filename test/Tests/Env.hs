{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Env (envTests) where

import Control.Exception (catch, try)
import Control.Lens
import Test.Tasty
import Test.Tasty.HUnit

import Language.Lean as Lean

-- | Validates that the IO action throws a Lean exception
expectLeanFailure :: String -> IO a -> IO ()
expectLeanFailure msg action = do
  expectLeanException msg =<< try action

-- | Validates that the IO action throws a Lean exception
expectLeanException :: String -> Either LeanException a -> IO ()
expectLeanException msg mr = do
  case mr of
    Left _ -> return ()
    Right _ -> assertFailure msg

envTests :: TestTree
envTests = testGroup "Env"
  [ testCase "add_univ" testAddUniv
  , testCase "standard_env" testStandardEnv
  , testCase "hott_env"     testHottEnv
  , testCase "add_decls"    testEnvAddDecls
  , testCase "is_descendant" testIsDescendant
  , testCase "id"       testId
  , testCase "import"   testImport
  ]

u0 :: Univ
u0 = explicitUniv 0

u1 :: Univ
u1 = explicitUniv 1

testAddUniv :: IO ()
testAddUniv = do
  env <- standardEnv trustHigh
  new_env <- envAddUniv "u" env
  assert (new_env      `envContainsUniv` "u")
  assert (not (new_env `envContainsUniv` "v"))
  assert (new_env^..envUnivs == ["u"])
  -- Check that adding duplicate names throws an exception.
  catch (envAddUniv "u" new_env
         >> assertFailure "Expected exception from duplicate universe addition")
        (\e -> assert (exceptionKind e == LeanKernelException))

testStandardEnv :: IO ()
testStandardEnv = do
  env <- standardEnv trustHigh
  assert $ envTrustLevel env == trustHigh
  assert $ envHasProofIrrelevantProp env
  assert $ envIsImpredicative env
  env4 <- standardEnv 4
  assert $ envTrustLevel env4 == 4

testHottEnv :: IO ()
testHottEnv = do
  env <- hottEnv trustHigh
  assert $ envTrustLevel env == trustHigh
  assert $ not $ envHasProofIrrelevantProp env
  assert $ not $ envIsImpredicative env
  env9 <- hottEnv 9
  assert $ envTrustLevel env9 == 9

testEnvAddDecls :: IO ()
testEnvAddDecls = do
  -- Declare Bar as a proposition.
  let bar = axiom "Bar" [] (sortExpr zeroUniv)
  -- Create an empty environment to certify Bar in
  env0 <- standardEnv trustHigh
  let Right certBar = tryCertify env0 bar
  envBar <- envAddCertDecl certBar env0

  -- Test failures
  expectLeanFailure "Expected duplicate add to fail" $ do
    envAddCertDecl certBar envBar
  expectLeanFailure "Expected add to new environment to fail" $ do
    envAddCertDecl certBar =<< standardEnv trustHigh
  expectLeanFailure "Expected add to forget environment to fail" $ do
    envAddCertDecl certBar =<< envForget env0

  let foo = axiom "foo" [] (constExpr "Bar" [])
  let Right certFoo = tryCertify envBar foo
  envBarFoo <- envAddCertDecl certFoo envBar

  assert $ envBarFoo `envIsDescendant` envBar

  let foo2 = axiom "foo2" [] (constExpr "Bar" [])
  let Right certFoo2 = tryCertify envBar foo2
  envBarFoo2 <- envAddCertDecl certFoo2 envBarFoo

  -- Test replace axiom
  let foo2Def = theoremWith envBarFoo "foo2" [] (constExpr "Bar" []) (constExpr "foo" [])
  let Right certFoo2Def = tryCertify envBarFoo foo2Def
  envFinal  <- envReplaceAxiom certFoo2Def envBarFoo2
  assert $ envIsDescendant envFinal envBarFoo2

--
testIsDescendant :: IO ()
testIsDescendant = do
  env0 <- standardEnv trustHigh
  assert $ env0 `envIsDescendant` env0

  do env_add_a0 <- envAddUniv "a" env0
     env_add_a1 <- envAddUniv "a" env0
     assert $ env_add_a0 `envIsDescendant` env0
     assert $ env_add_a1 `envIsDescendant` env0
     assert $ not $ env0 `envIsDescendant` env_add_a0
     assert $ not $ env0 `envIsDescendant` env_add_a1
     assert $ not $ env_add_a0 `envIsDescendant` env_add_a1

  do env_forget0 <- envForget env0
     env_forget1 <- envForget env0
     assert $ not $ env0 `envIsDescendant` env_forget0
     assert $ not $ env0 `envIsDescendant` env_forget1
     assert $ not $ env_forget0 `envIsDescendant` env_forget1

testId :: IO ()
testId = do
  env <- standardEnv trustHigh
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

  let Right id_cert_def = tryCertify env id_def
  new_env <- envAddCertDecl id_cert_def env

  assert $ not $ env `envContainsDecl` "id"
  assert $   new_env `envContainsDecl` "id"

  let decls = fmap declView $ new_env^..envDecls

  assert $ length decls == 1

  let prop = sortExpr u0

  let id1     = constExpr "id" [u1]
  let id1T1   = id1 `appExpr` prop
  let id1T1T0 = id1T1 `appExpr` sortExpr u1

  let tc = typechecker new_env
  let (n1, _s1) = whnf tc id1T1T0
  assert $ n1 == sortExpr u1

  let (n2, _s2) = inferType tc id1T1
  let (r,_cs) = isDefEq tc n2 (piExpr BinderDefault "a" prop prop)
  assert r

testImport :: IO ()
testImport = do
  env <- standardEnv trustHigh
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
