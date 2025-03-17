# CHANGELOG

### [Unreleased]

## [0.11.0] - 2023-12-16

### Updated

- **Breaking change**: While the public API has not changed, the underlying code
  has been re-written. By default, the parser code is much stricter and follows
  the XML specification more closely. For this release, the parser only has a
  strict mode. Future versions may have options to relax the parsing code and allow
  non-spec behavior.

### Fixed

- Fixed parsing of XML document declarations.
  Thanks [@Mingun](https://github.com/Mingun).
  See [Issue #9](https://github.com/bluk/maybe_xml/issues/9)

## [0.10.1] - 2023-12-01

### Fixed

- Fix incorrect parsing CDATA issue. The `pos` was not used when scanning for
  the CDATA.
  Thanks [@Mingun](https://github.com/Mingun).
  See [Issue #8](https://github.com/bluk/maybe_xml/issues/8)

## [0.10.0] - 2023-11-21

### Removed

- Removed deprecated functionality.

## [0.9.0] - 2023-11-14

### Updated

* **Breaking change**: `Lexer::into_inner()` returns a `&str`.
* **Breaking change**: `Lexer::tokenize()` restores the panic behavior in
  previous versions. If the position argument is greater than the input's byte length,
  then a panic occurs. The position argument is also immediately checked to be
  at a UTF-8 character boundary instead of waiting for a panic in a subsequent
  function call.
* **Breaking change**: Spaces around properties like attribute names, attribute
  values, and processing instructions are trimmed in their representations.
* **Breaking change**: Minimum supported Rust version is now `1.71.0`.
* Renamed `Lexer` into `Reader`. `Lexer` is still exposed as a hidden alias but will
  be removed in a future version.
* `Token::ty()` is made `const`.

### Removed

* Remove `From` and `from_str` implementations from token
  and property types. Due to possible future implementation changes and
  undocumented invariants, the ability to directly construct a type is removed.
* Remove `as_str()`, `as_bytes()`, `as_str_unchecked()`,
  and `into_inner()` implementations for `Attributes` and `Attribute`. Both are opaque
  types where the internal data may change.

### Deprecated

- Deprecated `from_slice_unchecked()` on `Lexer`. Use `core::str::from_utf8_unchecked()`
  on the byte slice and then pass into `Reader::from_str()` if the byte slice is
  guaranteed to be a valid UTF-8 string.

### Added

- Most functions are `const` on the token and property types. Refactored most
  token and property type function implementations to be `const` compatible.
- Add `Reader::parse()` as a `const` alternative function to
  `Reader::tokenize()`. The function does not mutate the position argument so the
  position must be re-calculated like `pos += token.len()` in the calling code.
- Add `Attributes.parse()` as a `const` function to parse the attributes like
  `Reader::parse()`.
- Add `Display` implementations to `Token` and token types.
- Derive `PartialOrd`, `Ord`, and `Hash` for `Token` and `Ty`.
- Add `len()` method to token types.

## [0.8.0] - 2023-11-11

### Added

- Add `Token::as_str()` and `as_str()` to other token and property types.
  `as_str()` replaces `to_str()` or `as_str_unchecked()` as the preferred method
  to get the underlying value.
- Add `Token::from_str()` and `from_str()` to other token and property types.
  The method should be used for constructing values in tests.

### Deprecated

- Deprecated `as_str_unchecked()`, `to_str()` and `into_inner()` on `Token`.
- Deprecated `as_str_unchecked()`, `to_str()` and `into_inner()` on token types.
- Deprecated `as_str_unchecked()` and `to_str()` on property types.

### Removed

- Remove `From<&[u8]>` and `AsRef<[u8]>` implementations from token and property
  types.

### Changed

- Changed the internal implementation of tokens to use `&str` instead of
  `&[u8]`. Micro benchmark performance should be similar to the previous
  implementation, but when using `as_str()` instead of `to_str()`, the calling
  code does not have to handle a `Result` type which should increase overall
  performance.

## [0.7.2] - 2023-11-01

### Changed

- Make `Iterator` from `Lexer::iter()` into an explicit type named `Iter`.

### Added

- Add `Lexer::into_inner()` to get the underlying bytes passed to the `Lexer`.
- Derive `PartialEq`, `Eq`, and `Hash` for `Lexer` and iterators.

## [0.7.1] - 2023-10-31

### Changed

- Modify `IntoIter` to be public.

## [0.7.0] - 2023-10-30

### Changed

- Add `Lexer::from_str` as the safe instantiation method.
- Refactor `Lexer::from_slice` to unsafe `Lexer::from_slice_unchecked`. It is
  assumed the byte slice is UTF-8 characters, and if they are not a valid slice
  of UTF-8 bytes, then the behavior is undefined.
- Dynamically determine `Token` type instead of storing the `Ty`. This reduces the
  `Token` size to 2 words (16 octets on a 64-bit machine).

### Removed

- Remove `offset` field from `Token`. If required, the offset should be
  calculated when tokens are consumed.
- Remove `scan` and `Scanner` from the public API.
- Remove `const` from `Token::ty()` method for possible future optimizations.
- Remove public `new` methods from `Token` and related types.

### Added

- Add unsafe `as_str_unchecked` to various types in cases where the bytes are
  guaranteed to be valid UTF-8.
- Add `as_bytes`, `to_str`, `as_str_unchecked`, and `into_inner` to `Token`.

## [0.6.0] - 2023-10-17

### Changed

- Change the minimum Rust supported version to `1.56.0` by moving benchmark code
  to own crate.

### Removed

- Removed deprecated functionality.

## [0.5.0] - 2023-09-28

### Added

- Add `Lexer` type which is the replacement for all evaluators. It borrows the
  input (a slice of bytes) and tokenizes with a borrowed `Token`. It has the
  benefits of both the existing evaluators (namely borrowing byte slices from the
  input so no allocations are necessary and allowing an `Iterator` API).
- Add `scan` function which scans for a token at the beginning of a slice.

### Deprecated

- Add deprecations to owned token types and all evaluators. The code will be
  removed in a future version.

### Fixed

- Fixed wrong lifetimes in the borrowed token types. The lifetimes should have
  been tied to the borrowed input instead of the token's type.

### Changed

- Derive most trait implementations for token and property types.
- Change the minimum Rust supported version to `1.60.0` by changing newer
  syntax code like `let...else` to older supported syntax.
- Add `#[must_use]` and `const` to functions.

### Removed

- Remove Token `From` implementations and other methods which are available
  when using `as_bytes()`.

## [0.4.0] - 2023-09-19

### Fixed

- Fix incorrect debug representation of token properties
  Thanks [@Mingun](https://github.com/Mingun).
  See [PR #3](https://github.com/bluk/maybe_xml/pull/3)
- Change word "token" to "token property" in documentation for token properties
  Thanks [@Mingun](https://github.com/Mingun).
  See [PR #4](https://github.com/bluk/maybe_xml/pull/4)
- Replace all occurrences of word `RecvEvaluator` with word `Evaluator`
  Thanks [@Mingun](https://github.com/Mingun).
  See [PR #5](https://github.com/bluk/maybe_xml/pull/5)

### Updated

- Remove unnecessary check for quote_state
  Thanks [@Mingun](https://github.com/Mingun).
  See [PR #6](https://github.com/bluk/maybe_xml/pull/6)
- Remove unnecessary parameters in various functions and simplify code.
  Thanks [@Mingun](https://github.com/Mingun).
  See [PR #7](https://github.com/bluk/maybe_xml/pull/7)

### Added

* Set Minimal Supported Rust Version (MSRV) to 1.70.0

## [0.3.0] - 2023-02-17

### Updated

* **Breaking change**: Rename `RecvEvaluator` and `RecvError` to
  `Evaluator` and `Error` respectively. Removes duplicatation of
  module name in type name.

## [0.2.0] - 2022-03-21

### Updated

* Add `ProcessingInstruction` `target` and `instructions` attributes
* Remove `non_exhaustive` from `Token` enum types.

## [0.1.0] - 2020-10-09

### Added

* Initial implementation.

[Unreleased]: https://github.com/bluk/maybe_xml/compare/v0.11.0...HEAD
[0.11.0]: https://github.com/bluk/maybe_xml/compare/v0.10.1...v0.11.0
[0.10.1]: https://github.com/bluk/maybe_xml/compare/v0.10.0...v0.10.1
[0.10.0]: https://github.com/bluk/maybe_xml/compare/v0.9.0...v0.10.0
[0.9.0]: https://github.com/bluk/maybe_xml/compare/v0.8.0...v0.9.0
[0.8.0]: https://github.com/bluk/maybe_xml/compare/v0.7.2...v0.8.0
[0.7.2]: https://github.com/bluk/maybe_xml/compare/v0.7.1...v0.7.2
[0.7.1]: https://github.com/bluk/maybe_xml/compare/v0.7.0...v0.7.1
[0.7.0]: https://github.com/bluk/maybe_xml/compare/v0.6.0...v0.7.0
[0.6.0]: https://github.com/bluk/maybe_xml/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/bluk/maybe_xml/compare/v0.4.0...v0.5.0
[0.4.0]: https://github.com/bluk/maybe_xml/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/bluk/maybe_xml/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/bluk/maybe_xml/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/bluk/maybe_xml/releases/tag/v0.1.0
