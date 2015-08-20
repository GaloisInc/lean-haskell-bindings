{-# LANGUAGE ForeignFunctionInterface #-}
module Language.Lean.Internal.String
  ( mkLeanString
  , mkLeanText
  , decodeLeanString
  , withLeanStringPtr
  , withLeanTextPtr
  , lean_string_del
  ) where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafePackCString, unsafeUseAsCString)
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Foreign.Ptr
import Foreign.C (CChar, CString)

#include "lean_string.h"

{#fun unsafe lean_string_del
  { `CString'
  } -> `()' #}

foreign import ccall "&lean_string_del"
  _lean_string_del_ptr :: FunPtr (CString -> IO ())

-- | This decodes a CString as Lean text
decodeLeanText :: CString -> IO Text
decodeLeanText cstr = do
  -- Get cstring as a bytestring
  bs <- unsafePackCString cstr
  -- Decode the CString (this will throw an exception is the string is not UTF8).
  return $! decodeUtf8 bs

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

-- | Use the string as a UTF8 CString
withLeanStringPtr :: String -> (CString -> IO a) -> IO a
withLeanStringPtr s f = withLeanTextPtr (fromString s) f

-- | Use the text as a UTF8 CString
withLeanTextPtr :: Text -> (CString -> IO a) -> IO a
withLeanTextPtr txt f =
  unsafeUseAsCString (encodeUtf8 txt `BS.snoc` 0) f
