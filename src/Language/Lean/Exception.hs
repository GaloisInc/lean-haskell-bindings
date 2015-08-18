module Language.Lean.Exception
  ( LeanException(..)
  , LeanExceptionKind(..)
  ) where


import Control.Exception
import Data.Typeable

-- | Information about the Kind of exception thrown.
data LeanExceptionKind
   = LeanSystemException
   | LeanOutOfMemory
   | LeanInterrupted
   | LeanKernelException
   | LeanOtherException
  deriving (Show)

-- | An exception thrown by Lean
data LeanException
   = LeanException { leanExceptionKind :: !LeanExceptionKind
                   , leanExceptionName :: !String
                   }
 deriving (Typeable, Show)

instance Exception LeanException
