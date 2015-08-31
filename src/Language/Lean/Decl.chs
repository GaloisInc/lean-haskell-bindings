module Language.Lean.Decl
  ( Env(..)
  , Decl
    -- * Constructors
  , axiomDecl
  , constDecl
  , defDecl
  , defWithDecl
  , thmDecl
--  , thmWithDecl
    -- * Projections
  , declName
  , declUnivParams
  , declType
  , DeclView(..)
  , viewDecl
    -- * Certified declarations
  , CertDecl
  , check
  ) where

import Language.Lean.Internal.Decl
