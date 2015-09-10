{-|
Module      : Language.Lean.Parser
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com

This provides functions for parsing Lean files, commands, and expressions.
-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Safe #-}
module Language.Lean.Parser
  ( parseFile
  , parseCommands
  , parseExpr
  ) where

import Foreign
import Foreign.C

import Language.Lean.List

{#import Language.Lean.Internal.Decl#}
{#import Language.Lean.Internal.Exception#}
{#import Language.Lean.Internal.Expr#}
{#import Language.Lean.Internal.IOS#}
{#import Language.Lean.Internal.Name#}
{#import Language.Lean.Internal.Options#}

#include "lean_bool.h"
#include "lean_macros.h"
#include "lean_exception.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"
#include "lean_expr.h"
#include "lean_decl.h"
#include "lean_ios.h"
#include "lean_parser.h"

-- | Parse the given file using the environment
--
-- This returns the new environment along with the options after any commands
-- modified it.
parseFile :: IOState tp -> Env -> FilePath -> IO (Env, Options)
parseFile s old_env path = do
  alloca $ \env_ptr -> do
    alloca $ \ios_ptr -> do
      runLeanPartialAction $ lean_parse_file old_env (someIOS s) path env_ptr ios_ptr
      new_env <- mkLeanValue =<< peek env_ptr
      new_ios <- mkLeanValue =<< peek ios_ptr
      new_ops <- getStateOptions new_ios
      seq new_env $ do
      seq new_ops $ do
      return $! (new_env, new_ops)

{#fun lean_parse_file
 { `Env'
 , `SomeIOState'
 , `String'
 , `OutEnvPtr'
 , `OutSomeIOStatePtr'
 , `OutExceptionPtr' } -> `Bool' #}


-- | Parse the given string as commands using the environment.
--
-- This returns the new environment along with the options after any commands
-- modified it.
parseCommands :: IOState tp -> Env -> String -> IO (Env, Options)
parseCommands s old_env cmds = do
  alloca $ \env_ptr -> do
    alloca $ \ios_ptr -> do
      runLeanPartialAction $ lean_parse_commands old_env (someIOS s) cmds env_ptr ios_ptr
      new_env <- mkLeanValue =<< peek env_ptr
      new_ios <- mkLeanValue =<< peek ios_ptr
      new_ops <- getStateOptions new_ios
      seq new_env $ do
      seq new_ops $ do
      return $! (new_env, new_ops)

{#fun lean_parse_commands
 { `Env'
 , `SomeIOState'
 , `String'
 , `OutEnvPtr'
 , `OutSomeIOStatePtr'
 , `OutExceptionPtr' } -> `Bool' #}

-- | Parse the given string as commands using the environment.
--
-- This returns the expression along with the universe parameters that were automatically
-- generated.
parseExpr :: IOState tp -> Env -> String -> IO (Expr, List Name)
parseExpr s old_env input = do
  alloca $ \expr_ptr -> do
    alloca $ \univ_ptr -> do
      runLeanPartialAction $ lean_parse_expr old_env (someIOS s) input expr_ptr univ_ptr
      new_expr <- mkLeanValue =<< peek expr_ptr
      new_univs <- mkLeanValue =<< peek univ_ptr
      seq new_expr $ do
      seq new_univs $ do
      return $! (new_expr, new_univs)

{#fun lean_parse_expr
 { `Env'
 , `SomeIOState'
 , `String'
 , `OutExprPtr'
 , `OutListNamePtr'
 , `OutExceptionPtr' } -> `Bool' #}
