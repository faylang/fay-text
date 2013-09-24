fay-text provides a Text whose values are represented as JavaScript strings for Fay, and as `Data.Text` for GHC. You can use this package instead of directly depending on `text` if you want to.

Fay has a special case that's applicable for `fay-text`. If a file has `{-# LANGUAGE OverloadedStrings #-}` GHC will replace all string literals with `fromString lit`. `{-# LANGUAGE RebindableSyntax #-}` tells GHC to use the `fromString` currently in scope instead of `Data.String.fromString`. With these two extensions enabled Fay outputs all string literals as JavaScript strings, which is the same representation as `Fay.Text` uses.

Note that you can mix modules using text literals and string literals, the behavior is local to the module.
