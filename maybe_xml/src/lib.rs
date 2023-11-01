//! `MaybeXml` is a library to scan and evaluate [XML][xml]-like data into tokens. In
//! effect, the library provides a non-validating lexer. The interface is similar to many
//! XML pull parsers.
//!
//! # Usage
//!
//! The library user creates a [`Lexer`] from a slice of bytes. The slice of
//! bytes is usually from a buffer managed by the library user. For instance, it
//! could be a buffer of data that is being read over a network socket, a memory
//! mapped file, or just a [`Vec`] of bytes.
//!
//! Then, the library user can call [`Lexer::tokenize()`][`Lexer::tokenize()`]
//! to try to get the next [`Token`][crate::token::Token]. If successful,
//! repeat calling `tokenize` and process the available tokens.
//!
//! Alternatively, the user can turn the `Lexer` into an iterator via
//! [`Lexer::iter()`][Lexer::iter()] or [`IntoIterator::into_iter()`].
//!
//! # Examples
//!
//! ## Using [`Iterator`] functionality
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
//! assert_eq!(token_type, Some(Ty::StartTag(StartTag::from("<id>"))));
//! match token_type {
//!     Some(Ty::StartTag(start_tag)) => {
//!         assert_eq!(start_tag.name().to_str()?, "id");
//!     }
//!     _ => panic!("unexpected token"),
//! }
//! assert_eq!(iter.next(), Some(Ty::Characters(Characters::from("Example"))));
//! assert_eq!(iter.next(), Some(Ty::EndTag(EndTag::from("</id>"))));
//! assert_eq!(iter.next(), None);
//! # Ok::<(), core::str::Utf8Error>(())
//! ```
//!
//! ## Using [`Lexer::tokenize()`][Lexer::tokenize()] directly
//!
//! ```
//! use maybe_xml::{Lexer, token::{Characters, EndTag, StartTag, Ty}};
//!
//! let mut buf = Vec::new();
//! // Note the missing closing tag character `>` in the end tag.
//! buf.extend(b"<id>123</id");
//!
//! let lexer = unsafe { Lexer::from_slice_unchecked(&buf) };
//! let mut pos = 0;
//!
//! let ty = lexer.tokenize(&mut pos).map(|token| token.ty());
//! assert_eq!(Some(Ty::StartTag(StartTag::from("<id>".as_bytes()))), ty);
//!
//! // Position was assigned to the index after the end of the token
//! assert_eq!(4, pos);
//!
//! let ty = lexer.tokenize(&mut pos).map(|token| token.ty());
//! assert_eq!(Some(Ty::Characters(Characters::from("123".as_bytes()))), ty);
//!
//! // Position was assigned to the index after the end of the token
//! assert_eq!(7, pos);
//!
//! let token = lexer.tokenize(&mut pos);
//! // The last token is incomplete because it is missing the `>`
//! assert_eq!(None, token);
//!
//! // Discard the tokenized input
//! buf.drain(..pos);
//! pos = 0;
//!
//! // Verify that the buffer is empty. If it is not empty, then there is data
//! // which could not be identified as a complete token. This usually indicates
//! // an error has occurred. If there is more data (say coming from a network
//! // socket), then append the new data when it becomes available and call
//! // tokenize again.
//! ```
//!
//! # Well-formed vs. Malformed document processing
//!
//! The library should scan and evaluate well-formed XML documents correctly. For
//! XML documents which are not well-formed, the behavior is currently undefined. The library
//! does not error when scanning a malformed document.
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

pub(crate) mod bytes;
mod lexer;
pub mod token;

pub use lexer::{IntoIter, Iter, Lexer};
