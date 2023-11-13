//! `MaybeXml` is a library to scan and evaluate [XML][xml]-like data into
//! tokens. In effect, the library provides a non-validating lexer. The
//! interface is similar to many XML pull parsers.
//!
//! # Usage
//!
//! The library user creates a [`Lexer`] from a `&str`.
//!
//! Then, the library user can call [`Lexer::tokenize()`][`Lexer::tokenize()`]
//! to try to get the next [`Token`][crate::token::Token]. If successful, repeat
//! calling `tokenize` and process the available tokens.
//!
//! Alternatively, the user can turn the `Lexer` into an iterator via
//! [`Lexer::iter()`][Lexer::iter()] or [`IntoIterator::into_iter()`].
//!
//! # Examples
//!
//! ## Using [`Lexer::tokenize()`][Lexer::tokenize()]
//!
//! ```
//! use maybe_xml::{Lexer, token::{Characters, EndTag, StartTag, Ty}};
//!
//! let input = "<id>123</id>";
//!
//! let lexer = Lexer::from_str(input);
//! let mut pos = 0;
//!
//! let token = lexer.tokenize(&mut pos);
//! assert_eq!(Some(Ty::StartTag(StartTag::from_str("<id>"))), token.map(|t| t.ty()));
//! assert_eq!(4, pos);
//!
//! let token = lexer.tokenize(&mut pos);
//! assert_eq!(Some(Ty::Characters(Characters::from_str("123"))), token.map(|t| t.ty()));
//! assert_eq!(7, pos);
//!
//! let token = lexer.tokenize(&mut pos);
//! assert_eq!(Some(Ty::EndTag(EndTag::from_str("</id>"))), token.map(|t| t.ty()));
//! assert_eq!(12, pos);
//!
//! let token = lexer.tokenize(&mut pos);
//! assert_eq!(None, token);
//!
//! // Verify that `pos` is equal to `input.len()` to ensure all data was
//! // processed.
//! ```
//!
//! //! ## Using [`Iterator`] functionality
//!
//! ```
//! use maybe_xml::{Lexer, token::{Characters, EndTag, StartTag, Ty}};
//!
//! let input = "<id>Example</id>";
//!
//! let lexer = Lexer::from_str(input);
//!
//! let mut iter = lexer.into_iter().map(|token| token.ty());
//!
//! let token_type = iter.next();
//! assert_eq!(token_type, Some(Ty::StartTag(StartTag::from_str("<id>"))));
//! match token_type {
//!     Some(Ty::StartTag(start_tag)) => {
//!         assert_eq!(start_tag.name().as_str(), "id");
//!     }
//!     _ => panic!("unexpected token"),
//! }
//! assert_eq!(Some(Ty::Characters(Characters::from_str("Example"))), iter.next());
//! assert_eq!(Some(Ty::EndTag(EndTag::from_str("</id>"))), iter.next());
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

mod lexer;
pub mod token;

pub use lexer::{IntoIter, Iter, Lexer};

enum QuoteState {
    None,
    Single,
    Double,
}
