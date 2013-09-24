{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PackageImports     #-}

-- | Module to be shared between server and client.
--
-- This module must be valid for both GHC and Fay.
--
-- For GHC this is an alias for Data.Text, for Fay it's an opaque data type represented by JavaScript strings.
--

module Fay.Text
  ( module Fay.Text.Type
  , empty
  , cons
  , snoc
  , append
  , uncons
  , head
  , last
  , tail
  , init
  , null
  , length
  , map
  , intercalate
  , intersperse
  , reverse
  , toLower
  , toUpper
  , concat
  , concatMap
  , any
  , all
  , maximum
  , minimum
  ) where

import           Data.Data
import           Fay.Text.Type
import           Prelude (Bool, Char, Int, Maybe)

#ifdef FAY
import           FFI

empty :: Text
empty = ffi "''"

cons :: Char -> Text -> Text
cons = ffi "%1 + %2"

snoc :: Text -> Char -> Text
snoc = ffi "%1 + %2"

append :: Text -> Text -> Text
append = ffi "%1 + %2"

uncons :: Text -> Maybe (Char, Text)
uncons = ffi "%1[0] ? { instance: 'Just', slot1 : [%1[0],%1.slice(1)] } : { instance : 'Nothing' }"

head :: Text -> Char
head = ffi "%1[0] || (function () {throw new Error('Fay.Text.head: empty Text'); }())"

last :: Text -> Char
last = ffi "%1.length ? %1[%1.length-1] : (function() { throw new Error('Fay.Text.last: empty Text') })()"

tail :: Text -> Text
tail = ffi "%1.length ? %1.slice(1) : (function () { throw new Error('Fay.Text.tail: empty Text') })()"

init :: Text -> Text
init = ffi "%1.length ? %1.slice(0,-1) : (function () { throw new Error('Fay.Text.init: empty Text') })()"

null :: Text -> Bool
null = ffi "!(%1.length)"

length :: Text -> Int
length = ffi "%1.length"

map :: (Char -> Char) -> Text -> Text
map = ffi "[].map.call(%2, %1).join('')"

intercalate :: Text -> [Text] -> Text
intercalate = ffi "%2.join(%1)"

intersperse :: Char -> Text -> Text
intersperse = ffi "%2.split('').join(%1)"

-- TODO transpose

reverse :: Text -> Text
reverse = ffi "%1.split('').reverse().join('')"


-- TODO replace: Can't use String:replace.

-- TODO toCaseFold

toLower :: Text -> Text
toLower = ffi "%1.toLowerCase()"

toUpper :: Text -> Text
toUpper = ffi "%1.toUpperCase()"

-- TODO justifyLeft, justifyRight, center

-- TODO foldl, foldl' foldl1', foldr, foldr1

concat :: [Text] -> Text
concat = ffi "%1.join('')"

concatMap :: (Char -> Text) -> Text -> Text
concatMap = ffi "[].map.call(%2, %1).join('')"

any :: (Char -> Bool) -> Text -> Bool
any = ffi "[].filter.call(%2, %1).length > 0"

all :: (Char -> Bool) -> Text -> Bool
all = ffi "[].filter.call(%2, %1).length == %1.length"

maximum :: Text -> Char
maximum = ffi "(function (s) { \
  \   if (s === '') { throw new Error('Fay.Text.maximum: empty string'); } \
  \   var max = s[0]; \
  \   for (var i = 1; i < s.length; s++) { \
  \     if (s[i] > max) { max = s[i]; } \
  \   } \
  \   return max; \
  \ })(%1)"

minimum :: Text -> Char
minimum = ffi "(function (s) { \
  \   if (s === '') { throw new Error('Fay.Text.maximum: empty string'); } \
  \   var min = s[0]; \
  \   for (var i = 1; i < s.length; s++) { \
  \     if (s[i] < min) { min = s[i]; } \
  \   } \
  \   return min; \
  \ })(%1)"

-- TODO scanl, scanl1, scanr, scanr1
-- TODO mapAccumL, mapAccumR

-- TODO replicate, unfoldr, unforldrN, take, drop, takeWhile, dropWhile, dropWhileEnd, dropAround, strip, stripStart, stripEnd, splitAt, breakOn, breakOnEnd, break, span, group, groupBy, inits, tails

-- TODO splitOn, split, chunksOf

-- TODO lines, words, unlines, unwords

-- TODO isPrefixOf, isSuffixOf, isInfixOf

-- TODO stripPrefix, stripSuffix, commonPrefixes

-- TODO filter, breakOnAll, find, partition

-- TODO index, findIndex, count

-- TODO zip, zipWith

#else
import qualified Data.Text as T

empty       = T.empty
cons        = T.cons
snoc        = T.snoc
append      = T.append
uncons      = T.uncons
head        = T.head
last        = T.last
tail        = T.tail
init        = T.init
null        = T.null
length      = T.length
map         = T.map
intercalate = T.intercalate
intersperse = T.intersperse
reverse     = T.reverse
toLower     = T.toLower
toUpper     = T.toUpper
concat      = T.concat
concatMap   = T.concatMap
any         = T.any
all         = T.all
maximum     = T.maximum
minimum     = T.minimum
#endif
