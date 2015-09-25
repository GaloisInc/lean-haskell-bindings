{-|
Module      : Language.Lean.Internal.Exception
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

Internal operations for working with Lean exceptions.

As exceptions are core to working with the Lean API, this module is imported
by many other modules.  However, to pretty print Lean exceptions, lean's
exception pretty printer expects both an 'IOState' and and 'Env' values
for the exception.  To accomodate this, this module delares several other
types used for pretty printing exceptions that appear in several modules,
and it defines the operations on these types needed to implement typeclass
instances.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_HADDOCK not-home #-}
module Language.Lean.Internal.Exception
  ( LeanException
  , LeanExceptionKind(..)
  , exceptionKind
  , exceptionMessage
  , exceptionMessageWithEnv
  , exceptionRawMessage
  , exceptionDetailedMessage
  , leanException
    -- * FFI types
  , ExceptionPtr
  , OutExceptionPtr
  , mkLeanException
  , mkLeanExceptionWithEnv
    -- * Partial operations
  , LeanPartialAction
  , runLeanPartialAction
  , LeanPartialFn
  , runLeanPartialFn
  , runLeanMaybeFn
  , IsLeanValue(..)
    -- * Options
  , Options
  , OptionsPtr
  , OutOptionsPtr
  , withOptions
  , emptyOptions
  , joinOptions
    -- * Env
  , Env
  , EnvPtr
  , OutEnvPtr
  , withEnv
    -- * IOS
  , type IOStateType(..)
  , IOState
  , withIOState
  , someIOS
  , SomeIOState
  , SomeIOStatePtr
  , OutSomeIOStatePtr
  , withSomeIOState
  , mkBufferedIOStateWithOptions
    -- * Functions that return a result in IO
  , tryAllocLeanValue
  ) where

import Control.Exception
import Control.Monad (when)
import Data.Typeable
import Foreign
import Foreign.C
import System.IO.Unsafe

import Language.Lean.Internal.String

#include "lean_macros.h"
#include "lean_exception.h"

-- We import IOS options as it provides a way of pretty printing.
#include "lean_bool.h"
#include "lean_name.h"
#include "lean_options.h"
#include "lean_univ.h"
#include "lean_expr.h"
#include "lean_decl.h"
#include "lean_env.h"
#include "lean_ios.h"

------------------------------------------------------------------------
-- Options declaration

-- Use nocode and manually generate for haddock.
{#pointer lean_options as Options foreign newtype nocode#}

-- | A set of Lean configuration options
newtype Options = Options (ForeignPtr Options)

-- | Function @c2hs@ uses to pass @Options@ values to Lean
withOptions :: Options -> (Ptr Options -> IO a) -> IO a
withOptions (Options o) = withForeignPtr $! o

-- | Haskell type for @lean_options@ FFI parameters.
{#pointer lean_options as OptionsPtr -> Options#}
-- | Haskell type for @lean_options*@ FFI parameters.
{#pointer *lean_options as OutOptionsPtr -> OptionsPtr #}

foreign import ccall unsafe "&lean_options_del"
  lean_options_del_ptr :: FunPtr (OptionsPtr -> IO ())

instance IsLeanValue Options (Ptr Options) where
  mkLeanValue = fmap Options . newForeignPtr lean_options_del_ptr

------------------------------------------------------------------------
-- Env declaration

{#pointer lean_env as Env foreign newtype nocode#}

-- | A Lean environment
newtype Env = Env (ForeignPtr Env)

-- | Function @c2hs@ uses to pass 'Env' values to Lean
withEnv :: Env -> (Ptr Env -> IO a) -> IO a
withEnv (Env o) = withForeignPtr $! o

-- | Haskell type for @lean_env@ FFI parameters.
{#pointer lean_env as EnvPtr -> Env#}
-- | Haskell type for @lean_env*@ FFI parameters.
{#pointer *lean_env as OutEnvPtr -> EnvPtr#}


------------------------------------------------------------------------
-- SomeIOState

-- | Internal state used for bindings
newtype SomeIOState = SomeIOState (ForeignPtr SomeIOState)

foreign import ccall unsafe "&lean_ios_del"
  lean_ios_del_ptr :: FunPtr (Ptr SomeIOState -> IO ())

{#pointer lean_ios as SomeIOState foreign newtype nocode#}

-- | Function @c2hs@ uses to pass 'SomeIOState' values to Lean
withSomeIOState :: SomeIOState -> (Ptr SomeIOState -> IO a) -> IO a
withSomeIOState (SomeIOState p) f = seq p $ withForeignPtr p f

-- | Haskell type for @lean_ios@ FFI parameters.
type SomeIOStatePtr = Ptr SomeIOState

-- | Haskell type for @lean_ios*@ FFI parameters.
{#pointer *lean_ios as OutSomeIOStatePtr -> SomeIOStatePtr #}

------------------------------------------------------------------------
-- IOState

-- | This describes the type of the @IOState@.
data IOStateType
   = Standard -- ^ A standard 'IOState'
   | Buffered -- ^ A buffered 'IOState'

-- | The IO State object
--
-- Lean uses two channels for sending output to the user:
--
--  * A /regular/ output channel, which consists of messages normally
--    printed to 'stdout'.
--  * A /diagnostic/ output channel, which consists of debugging
--    messages that are normally printed to 'stderr'.
--
-- This module currently provides two different 'IOState' types:
--
--  * A /standard/ IO state that sends regular output to 'stdout' and
--    diagnostic output to 'stderr'.
--  * A /buffered/ IO state type that stores output internally, and
--    provides methods for getting output as strings.
--
-- To prevent users from accidentally using the wrong type of output,
-- the 'IOState' has an extra type-level parameter used to
-- indicate the type of channel.  Most Lean operations support both
-- types of channels and either can be used.  Operations specific
-- to a particular channel can use this type parameter to ensure
-- users do not call the function on the wrong type of channel.  In
-- addition, we provide a function @stateTypeRepr@ to allow users
-- to determine the type of channel.
newtype IOState (tp :: IOStateType) = IOState (ForeignPtr SomeIOState)

-- | Run a computation with an io state.
withIOState :: IOState tp -> (Ptr SomeIOState -> IO a) -> IO a
withIOState (IOState ptr) f = seq ptr $ withForeignPtr ptr f

-- | Lift an arbitray IOState to SomeIOState
someIOS :: IOState tp -> SomeIOState
someIOS (IOState p) = SomeIOState (castForeignPtr p)

------------------------------------------------------------------------
-- LeanExceptionKind

-- | Information about the Kind of exception thrown.
data LeanExceptionKind
   = LeanSystemException
     -- ^ Exception generated by the C++ runtime
   | LeanOutOfMemory
     -- ^ Exception thrown when out of memory
   | LeanInterrupted
   | LeanKernelException
     -- ^ An exception thrown when a precondition is violated.
   | LeanUnifierException
   | LeanTacticException
   | LeanParserException
   | LeanOtherException
  deriving (Eq, Show)

{#enum lean_exception_kind as ExceptionKind { upcaseFirstLetter }
         deriving (Eq)#}

getLeanExceptionKind :: ExceptionKind -> LeanExceptionKind
getLeanExceptionKind k = do
  case k of
    LEAN_NULL_EXCEPTION    -> error "getLeanException not given an exception"
    LEAN_SYSTEM_EXCEPTION  -> LeanSystemException
    LEAN_OUT_OF_MEMORY     -> LeanOutOfMemory
    LEAN_INTERRUPTED       -> LeanInterrupted
    LEAN_KERNEL_EXCEPTION  -> LeanKernelException
    LEAN_UNIFIER_EXCEPTION -> LeanUnifierException
    LEAN_TACTIC_EXCEPTION  -> LeanTacticException
    LEAN_PARSER_EXCEPTION  -> LeanParserException
    LEAN_OTHER_EXCEPTION   -> LeanOtherException

------------------------------------------------------------------------
-- FFI Declarations

-- | An exception thrown by Lean
data LeanException
   = BindingsLeanException !LeanExceptionKind !String
     -- ^ This is an exception generated by the bindings.
   | RealLeanException !(ForeignPtr LeanException)
     -- ^ This is an exception generated by Lean
   | PrettyLeanException !Env !Options !(ForeignPtr LeanException)
     -- ^ This is an exception generated by Lean in a context that has
     -- an associated IOState
 deriving (Typeable)

instance Show LeanException where
  show e =
    "leanException " ++ show (exceptionKind e)
              ++ " " ++ show (exceptionMessage e)

instance Exception LeanException

-- | Pointer used as input parameter for exceptions in FFI bindings
{#pointer lean_exception as ExceptionPtr -> LeanException#}
-- | Pointer used as output parameter for exceptions in FFI bindings
{#pointer *lean_exception as OutExceptionPtr -> ExceptionPtr #}

-- | Create a Lean exception with the given kind and message.
leanException :: LeanExceptionKind -> String -> LeanException
leanException = BindingsLeanException

-- | Create a Lean exception from a pointer.
mkLeanException :: Ptr LeanException -> IO LeanException
mkLeanException = fmap RealLeanException . newForeignPtr lean_exception_del_ptr

-- | Create a Lean exception from a pointer.
mkLeanExceptionWithEnv :: Env -> Options -> Ptr LeanException -> IO LeanException
mkLeanExceptionWithEnv e s p = do
  PrettyLeanException e s <$> newForeignPtr lean_exception_del_ptr p

foreign import ccall unsafe "&lean_exception_del"
  lean_exception_del_ptr :: FunPtr (ExceptionPtr -> IO ())

-- | Get the kind of this exception.
exceptionKind :: LeanException -> LeanExceptionKind
exceptionKind (BindingsLeanException k _) = k
exceptionKind (RealLeanException fnPtr) =
  leanExceptionPtrKind fnPtr
exceptionKind (PrettyLeanException _ _ fnPtr) =
  leanExceptionPtrKind fnPtr

leanExceptionPtrKind :: ForeignPtr LeanException -> LeanExceptionKind
leanExceptionPtrKind fnPtr =
  getLeanExceptionKind $
    unsafePerformIO $
      withForeignPtr fnPtr $ lean_exception_get_kind

{#fun unsafe lean_exception_get_kind
 { `ExceptionPtr' } -> `ExceptionKind' #}

-- | Get basic information describing this exception.
exceptionRawMessage :: LeanException -> String
exceptionRawMessage (BindingsLeanException _ msg) = msg
exceptionRawMessage (RealLeanException fnPtr) =
  leanExceptionPtrMessage fnPtr
exceptionRawMessage (PrettyLeanException _ _ fnPtr) =
  leanExceptionPtrMessage fnPtr

-- | Get detailed information describing this exception.
exceptionDetailedMessage :: LeanException -> String
exceptionDetailedMessage (BindingsLeanException _ msg) = msg
exceptionDetailedMessage (RealLeanException fnPtr) =
  leanExceptionPtrDetailedMessage fnPtr
exceptionDetailedMessage (PrettyLeanException _ _ fnPtr) =
  leanExceptionPtrDetailedMessage fnPtr

-- | Get as pretty a message as possible from the LeanException
exceptionMessageWithEnv :: Env -> Options -> LeanException -> String
exceptionMessageWithEnv _  _ (BindingsLeanException _ msg)= msg
exceptionMessageWithEnv e o (RealLeanException fnPtr) =
  leanExceptionPtrPrettyMessage e o fnPtr
exceptionMessageWithEnv e o (PrettyLeanException _ _ fnPtr) =
  leanExceptionPtrPrettyMessage e o fnPtr

-- | Get the messapretty a message as possible from the LeanException
exceptionMessage :: LeanException -> String
exceptionMessage (BindingsLeanException _ msg) = msg
exceptionMessage (RealLeanException fnPtr) =
  leanExceptionPtrMessage fnPtr
exceptionMessage (PrettyLeanException e o fnPtr) =
  leanExceptionPtrPrettyMessage e o fnPtr

leanExceptionPtrMessage :: ForeignPtr LeanException -> String
leanExceptionPtrMessage fnPtr = unsafePerformIO $ do
  withForeignPtr fnPtr $ lean_exception_get_message

leanExceptionPtrDetailedMessage :: ForeignPtr LeanException -> String
leanExceptionPtrDetailedMessage fnPtr = unsafePerformIO $ do
  withForeignPtr fnPtr $ lean_exception_get_detailed_message

leanExceptionPtrPrettyMessage :: Env -> Options -> ForeignPtr LeanException -> String
leanExceptionPtrPrettyMessage e o fnPtr = unsafePerformIO $ do
  ios <- mkBufferedIOStateWithOptions o
  withForeignPtr fnPtr $ \p -> do
    tryAllocLeanValue $ lean_exception_to_pp_string e (someIOS ios) p

{#fun unsafe lean_exception_get_message
 { `ExceptionPtr' } -> `String' getLeanString* #}

{#fun unsafe lean_exception_get_detailed_message
 { `ExceptionPtr' } -> `String' getLeanString* #}

{#fun unsafe lean_exception_to_pp_string
  { `Env'
  , `SomeIOState'
  , `ExceptionPtr'
  , id `Ptr CString'
  , `OutExceptionPtr'
  } -> `Bool' #}

------------------------------------------------------------------------
-- IsLeanValue

-- | Typeclass that associates Haskell types with their type in the FFI layer.
class Storable p => IsLeanValue v p | v -> p where
  -- | Create a Haskell value from a FFI value.
  mkLeanValue :: p -> IO v

instance IsLeanValue Bool CInt where
  mkLeanValue = return . toEnum . fromIntegral

instance IsLeanValue Word32 CUInt where
  mkLeanValue (CUInt x) = return x

instance IsLeanValue Int32 CInt where
  mkLeanValue (CInt x) = return x

instance IsLeanValue Double CDouble where
  mkLeanValue (CDouble d) = return d

instance IsLeanValue String CString where
  mkLeanValue = getLeanString

instance IsLeanValue (IOState tp) (Ptr SomeIOState) where
  mkLeanValue = fmap IOState . newForeignPtr lean_ios_del_ptr

instance IsLeanValue LeanException (Ptr LeanException) where
  mkLeanValue = mkLeanException

instance IsLeanValue Env (Ptr Env) where
   mkLeanValue = \v -> fmap Env $ newForeignPtr lean_env_del_ptr v

foreign import ccall unsafe "&lean_env_del"
  lean_env_del_ptr :: FunPtr (EnvPtr -> IO ())

------------------------------------------------------------------------
-- Partial functions

-- | A lean partial function is an action that may fail
type LeanPartialAction = (Ptr ExceptionPtr -> IO Bool)

-- | Run a lean partial action, throwing an exception if it fails.
runLeanPartialAction :: LeanPartialAction -> IO ()
runLeanPartialAction action =
  alloca $ \ex_ptr -> do
    poke ex_ptr nullPtr
    success <- action ex_ptr
    when (not success) $ do
      throwIO =<< mkLeanException =<< peek ex_ptr
{-# INLINE runLeanPartialAction #-}

-- | A lean partial function is a function that returns a value of type @a@, but
-- may fail.
type LeanPartialFn a = (Ptr a -> LeanPartialAction)

-- | Run a lean partial function
runLeanPartialFn :: Storable a
                 => LeanPartialFn a
                 -> IO a
runLeanPartialFn alloc_fn =
  alloca $ \ret_ptr -> do
    runLeanPartialAction (alloc_fn ret_ptr)
    peek ret_ptr
{-# INLINE runLeanPartialFn #-}

-- | Run a lean partial function where false does not automatically imply
-- an exception was thrown.
runLeanMaybeFn :: Storable p
               => LeanPartialFn p
               -> IO (Maybe p)
runLeanMaybeFn alloc_fn =
  alloca $ \ret_ptr -> do
    alloca $ \ex_ptr -> do
      poke ex_ptr nullPtr
      success <- alloc_fn ret_ptr ex_ptr
      if success then do
        r <- peek ret_ptr
        return $! Just r
      else do
        ptr <- peek ex_ptr
        if ptr == nullPtr then
          return $! Nothing
        else
          throwIO =<< mkLeanException ptr
{-# INLINE runLeanMaybeFn #-}

-- | Try to run a Lean partial function that returns a Lean value
-- that will need to be freed.
tryAllocLeanValue :: IsLeanValue a p
                  => LeanPartialFn p
                  -> IO a
tryAllocLeanValue = \alloc_fn -> mkLeanValue =<< runLeanPartialFn alloc_fn
{-# INLINE tryAllocLeanValue #-}

------------------------------------------------------------------------
-- Options Monoid instance

instance Monoid Options where
  mempty  = emptyOptions
  mappend = joinOptions

-- | An empty set of options
emptyOptions :: Options
emptyOptions = unsafePerformIO $ do
  tryAllocLeanValue lean_options_mk_empty

-- | Combine two options where the assignments from the second
-- argument override the assignments from the first.
joinOptions :: Options -> Options -> Options
joinOptions x y = unsafePerformIO $ do
  tryAllocLeanValue $ lean_options_join x y

{#fun unsafe lean_options_mk_empty
  { `OutOptionsPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

{#fun unsafe lean_options_join
  { `Options'
  , `Options'
  , `OutOptionsPtr'
  , `OutExceptionPtr'
  } -> `Bool' #}

------------------------------------------------------------------------
-- Options Eq instance

instance Eq Options where
  (==) = lean_options_eq

{#fun pure unsafe lean_options_eq
  { `Options', `Options' } -> `Bool' #}

------------------------------------------------------------------------
-- Options Show instance

instance Show Options where
  show = showOption

showOption :: Options -> String
showOption x = unsafePerformIO $ do
  tryAllocLeanValue $ lean_options_to_string x

{#fun unsafe lean_options_to_string
  { `Options'
  , id `Ptr CString'
  , `OutExceptionPtr'
  } -> `Bool' #}

------------------------------------------------------------------------
-- BufferedIOState

-- | Create IO state object that sends the regular and diagnostic output to
-- string buffers with the given options.
mkBufferedIOStateWithOptions :: Options -> IO (IOState 'Buffered)
mkBufferedIOStateWithOptions o = tryAllocLeanValue $ lean_ios_mk_buffered o

{#fun unsafe lean_ios_mk_buffered
 { `Options', `OutSomeIOStatePtr', `OutExceptionPtr' } -> `Bool' #}
