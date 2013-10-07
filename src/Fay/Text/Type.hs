{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls     #-}

-- | Module to be shared between server and client.
--
-- This module must be valid for both GHC and Fay.
--
-- For GHC this is an alias for Data.Text, for Fay it's an opaque data type represented by JavaScript strings.
--
module Fay.Text.Type
  ( Text
  , pack
  , unpack
  , fromString
  ) where

import           Prelude
import           Data.Data
#ifdef FAY
import           FFI

data Text
  deriving (Data, Eq, Ord, Read, Show, Typeable)

pack :: String -> Text
pack = ffi "%1"

unpack :: Text -> String
unpack = ffi "%1"

#else

import qualified Data.Text as T

type Text = T.Text

pack :: String -> Text
pack = T.pack

unpack :: Text -> String
unpack = T.unpack

#endif

-- | Have this in scope with the OverloadedStrings and BindableSyntax extensions
-- and Fay will replace all string literals with Text.
fromString :: String -> Text
fromString = pack
