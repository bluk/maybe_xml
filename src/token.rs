// Copyright 2022 Bryant Luk
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Tokens are the returned values when evaluating scanned byte ranges.
//!
//! There are two types of tokens: `borrowed` and `owned`. Borrowed
//! tokens re-use an internal buffer to temporarily represent a token
//! value. Owned tokens have independent buffers which store the
//! the token value.
//!
//! Methods which return a borrowed token act like a cursor API which
//! provides a temporary read only view of the scanned bytes. It should
//! perform better with less memory requirements.
//!
//! Methods which return an owned token act like an iterator API which
//! provides a value which can be easily stored or transformed. It should
//! be easier to use but at the cost of performance and more
//! memory usage.
//!
//! There may be "properties" of a token. For instance, a start tag
//! should have a tag name and possibly attributes. Tokens have
//! methods which return types in the `props` module. Properties
//! are borrowed views into a token.

pub mod borrowed;
#[cfg(any(feature = "alloc", feature = "std"))]
#[deprecated(since = "0.5.0", note = "Use Lexer with borrowed type views.")]
pub mod owned;
pub mod prop;

#[inline]
fn is_space(byte: u8) -> bool {
    matches!(byte, 32 | 9 | 13 | 10)
}
