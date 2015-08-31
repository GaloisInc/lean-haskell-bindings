{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Runners.AntXML

import Tests.Name (nameTests)
import Tests.Options (optionsTests)
import Tests.Univ (univTests)
import Tests.Expr (exprTests)
import Tests.Env  (envTests)

main :: IO ()
main = defaultMainWithIngredients ingrs tests

ingrs :: [Ingredient]
ingrs = antXMLRunner : defaultIngredients

tests :: TestTree
tests = testGroup "lean-bindings"
  [ nameTests
  , optionsTests
  , univTests
  , exprTests
  , envTests
  ]
