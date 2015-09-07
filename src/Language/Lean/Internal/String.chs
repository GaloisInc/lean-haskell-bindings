{-|
Module      : Language.Lean.Internal.String
Copyright   : (c) Galois Inc, 2015
License     : Apache-2
Maintainer  : jhendrix@galois.com, lcasburn@galois.com

This module exports functions for marshalling strings across
the Lean FFI.
-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Trustworthy #-}
module Language.Lean.Internal.String
  ( mkLeanString
  , mkLeanText
  , getLeanString
  , decodeLeanString
  , withLeanStringPtr
  , withLeanTextPtr
  ) where

import Control.Exception (bracket, finally, throwIO)
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafePackCString, unsafeUseAsCString)
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Foreign.Ptr
import Foreign.C (CChar, CString)

#include "lean_string.h"

{#fun unsafe lean_string_del
  { `CString' } -> `()' #}

-- | This decodes a CString as Lean text
decodeLeanText :: CString -> IO Text
decodeLeanText cstr = do
  -- Get cstring as a bytestring
  bs <- unsafePackCString cstr
  -- Decode the CString (this will throw an exception is the string is not UTF8).
  case decodeUtf8' bs of
    Left e -> throwIO e
    Right v -> return v

-- | This decodes a CString as Lean text
decodeLeanString :: CString -> IO String
decodeLeanString cstr = Text.unpack <$> decodeLeanText cstr

-- | This calls a function that allocates a Lean string, parses it as
-- a @Text@ value, and frees the string.
mkLeanText :: IO CString -> IO Text
mkLeanText alloc = bracket alloc lean_string_del $ decodeLeanText

-- | This calls a function that allocates a Lean string, parses it as
-- a @Text@ value, and frees the string.
mkLeanString :: IO CString -> IO String
mkLeanString alloc = bracket alloc lean_string_del $ decodeLeanString

-- | This calls a function that allocates a Lean string, parses it as
-- a @Text@ value, and frees the string.
getLeanString :: CString -> IO String
getLeanString ptr = decodeLeanString ptr `finally` lean_string_del ptr

-- | Use the string as a Lean string
withLeanStringPtr :: String -> (CString -> IO a) -> IO a
withLeanStringPtr s f = withLeanTextPtr (fromString s) f

-- | Use the text as a Lean string
withLeanTextPtr :: Text -> (CString -> IO a) -> IO a
withLeanTextPtr txt f =
  unsafeUseAsCString (encodeUtf8 txt `BS.snoc` 0) f
