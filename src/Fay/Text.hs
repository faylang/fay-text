{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE NoImplicitPrelude  #-}

-- | Module to be shared between server and client.
--
-- This module must be valid for both GHC and Fay.
--
-- For GHC this is an alias for Data.Text, for Fay it's an opaque data type represented by JavaScript strings.
--

module Fay.Text
  -- Only export what's defined in fay-base's Data.Text for both Fay and GHC
  ( Text
  -- * Creation and elimination
  , pack
  , unpack
  , fromString
  , empty
  -- * Breaking into many substrings
  , splitOn
  , stripSuffix
  -- * Basic interface
  , cons
  , snoc
  , append
  , uncons
  , head
  , init
  , last
  , tail
  , null
  , length
  -- * Special folds
  , maximum
  , all
  , any
  , concatMap
  , concat
  , minimum
  -- * Case conversion
  , toLower
  , toUpper
  -- * Transformations
  , map
  , intercalate
  , intersperse
  , reverse
  -- * Predicates
  , isPrefixOf
  -- * Substrings
  , drop
  , take
  -- * Breaking into lines and words
  , unlines
  , lines
  ) where

#ifdef FAY
import "fay-base" Data.Text
#else
import "text" Data.Text
#endif

import Fay.Text.Type (fromString)
