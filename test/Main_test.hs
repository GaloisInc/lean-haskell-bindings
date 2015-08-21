{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Runners.AntXML

import Tests.Name (nameTests)

main :: IO ()
main = defaultMainWithIngredients ingrs tests

ingrs :: [Ingredient]
ingrs = antXMLRunner : defaultIngredients

tests :: TestTree
tests = testGroup "lean-bindings"
  [ nameTests
  ]
