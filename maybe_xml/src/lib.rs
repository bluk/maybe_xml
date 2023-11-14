//! `MaybeXml` is a library to scan and evaluate [XML][xml]-like data into
//! tokens. In effect, the library provides a non-validating parser. The
//! interface is similar to many XML pull parsers.
//!
//! # Examples
//!
//! ## Using [`tokenize()`][Reader::tokenize()]
//!
//! ```
//! use maybe_xml::{Reader, token::{Characters, EndTag, StartTag, Ty}};
//!
//! let input = "<id>123</id>";
//!
//! let reader = Reader::from_str(input);
//! let mut pos = 0;
//!
//! let token = reader.tokenize(&mut pos);
//! if let Some(Ty::StartTag(tag)) = token.map(|t| t.ty()) {
//!     assert_eq!("id", tag.name().local().as_str());
//!     assert_eq!(None, tag.name().namespace_prefix());
//! } else {
//!     panic!();
//! }
//! assert_eq!(4, pos);
//!
//! let token = reader.tokenize(&mut pos);
//! if let Some(Ty::Characters(chars)) = token.map(|t| t.ty()) {
//!     assert_eq!("123", chars.content().as_str());
//! } else {
//!     panic!();
//! }
//! assert_eq!(7, pos);
//!
//! let token = reader.tokenize(&mut pos);
//! if let Some(Ty::EndTag(tag)) = token.map(|t| t.ty()) {
//!     assert_eq!("</id>", tag.as_str());
//!     assert_eq!("id", tag.name().local().as_str());
//! } else {
//!     panic!();
//! }
//! assert_eq!(12, pos);
//!
//! let token = reader.tokenize(&mut pos);
//! assert_eq!(None, token);
//!
//! // Verify that `pos` is equal to `input.len()` to ensure all data was
//! // processed.
//! ```
//!
//! ## Using [`Iterator`] functionality
//!
//! ```
//! use maybe_xml::{Reader, token::{Characters, EndTag, StartTag, Ty}};
//!
//! let input = "<id>Example</id>";
//!
//! let reader = Reader::from_str(input);
//!
//! let mut iter = reader.into_iter().map(|token| token.ty());
//!
//! if let Some(Ty::StartTag(start_tag)) = iter.next() {
//!     assert_eq!("id", start_tag.name().as_str(), "id");
//! } else {
//!     panic!();
//! }
//!
//! if let Some(Ty::Characters(chars)) = iter.next() {
//!     assert_eq!("Example", chars.content().as_str());
//! } else {
//!     panic!();
//! }
//!
//! if let Some(Ty::EndTag(tag)) = iter.next() {
//!     assert_eq!("</id>", tag.as_str());
//!     assert_eq!("id", tag.name().local().as_str());
//! } else {
//!     panic!();
//! }
//! assert_eq!(None, iter.next());
//! ```
//!
//! # Well-formed vs. Malformed document processing
//!
//! The library should scan and evaluate well-formed XML documents correctly.
//! For XML documents which are not well-formed, the behavior is currently
//! undefined. The library does not error when scanning a malformed document.
//!
//! # Security Considerations
//!
//! The input is managed by the library user. If there is malformed input, the
//! tokenizing functions could never return a complete token.
//!
//! For instance, the input could start with a `<` but there is no closing `>`
//! character.
//!
//! In particular, if data is coming over the network and the data is being
//! stored in a buffer, the buffer may have unbounded growth if the buffer's
//! data is freed only if a complete token is found.
//!
//! [xml]: https://www.w3.org/TR/2006/REC-xml11-20060816/

#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![warn(
    missing_copy_implementations,
    missing_debug_implementations,
    missing_docs,
    rust_2018_idioms,
    unused_lifetimes,
    unused_qualifications
)]

mod read;
pub mod token;

pub use read::{IntoIter, Iter, Reader};

#[doc(hidden)]
#[deprecated(since = "0.9.0", note = "Use Reader type instead.")]
pub use read::Reader as Lexer;

enum QuoteState {
    None,
    Single,
    Double,
}

#[allow(clippy::cast_possible_wrap)]
#[inline]
#[must_use]
const fn is_utf8_boundary(byte: u8) -> bool {
    byte as i8 >= -0x40
}
