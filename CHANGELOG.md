## Changelog

### fay-text 0.2.0.0

* Added a (large) subset of functions from `Data.Text`. These use the FFI so they should be pretty fast.
* Added `Eq` instance for `Text`.
* The `Text` type has been moved to `Fay.Text.Type` (re-exported by `Fay.Text`) in case you only want the type.

### fay-text 0.1.0.0 (2013-08-27)

* Initial release
