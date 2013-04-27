{-# Language EmptyDataDecls, NoImplicitPrelude #-}

module Fay.Text where

import Prelude
import FFI
import Data.Data

data Text = Text

pack :: String -> Text
pack = ffi "%1"

unpack :: Text -> String
unpack = ffi "%1"
