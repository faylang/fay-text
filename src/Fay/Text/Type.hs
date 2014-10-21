{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE PackageImports     #-}

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

#ifdef FAY
import "fay-base" Data.Text (Text, pack, unpack)
#else
import "text" Data.Text (Text, pack, unpack)
#endif

-- | Have this in scope with the OverloadedStrings and BindableSyntax extensions
-- and Fay will replace all string literals with Text.
fromString :: String -> Text
fromString = pack
