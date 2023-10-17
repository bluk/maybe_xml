# CHANGELOG

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

[Unreleased]: https://github.com/bluk/maybe_xml/compare/v0.6.0...HEAD
[0.6.0]: https://github.com/bluk/maybe_xml/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/bluk/maybe_xml/compare/v0.4.0...v0.5.0
[0.4.0]: https://github.com/bluk/maybe_xml/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/bluk/maybe_xml/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/bluk/maybe_xml/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/bluk/maybe_xml/releases/tag/v0.1.0