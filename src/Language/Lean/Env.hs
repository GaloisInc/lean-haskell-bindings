module Language.Lean.Env
  ( Env
  , TrustLevel
  , trustHigh
    -- * Constructing and manipulating environments.
  , stdEnv
  , hottEnv
  , envAddUniv
  , envAddDecl
  , envReplaceAxiom
    -- * Projections
  , envTrustLevel
  , envContainsProofIrrelProp
  , envIsImpredicative
  , envContainsUniv
  , envLookupDecl
  , envIsDescendant
    -- * Operations
  , envForget
  , envFoldDecls
  , envFoldUnivs
  ) where

import Language.Lean.Internal.Env
