// Copyright 2022 Bryant Luk
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! MaybeXml is a library to scan and evaluate [XML][xml]-like data into tokens. In
//! effect, the library provides a non-validating lexer. The interface is similar to many
//! XML pull parsers.
//!
//! The library does 3 things:
//!
//! 1. A `Scanner` receives byte slices and identifies the start and end of tokens like
//!    tags, character content, and declarations.
//!
//! 2. An `Evaluator` transforms bytes from an input source (like instances of types which implement
//!    `std::io::BufRead`) into complete tokens via either a cursor or an iterator pull
//!    style API.
//!
//!    From an implementation point of view, when a library user asks an
//!    `Evaluator` for the next token, the `Evaluator` reads the input and passes the
//!    bytes to an internal `Scanner`. The `Evaluator` buffers the scanned bytes and keeps reading
//!    until the `Scanner` determines a token has been completely read. Then all of the bytes
//!    which represent the token are returned to the library user as a variant of a token type.
//!
//! 3. Each token type provides methods which can provide views into the underlying bytes.
//!    For instance, a tag token could provide a `name()` method which returns a `TagName`.
//!    The `TagName` provides a method like `to_str()` which can be called to get a `str`
//!    representation of the tag name.
//!
//! # Usage
//!
//! In most cases, a library user should use an `Evaluator` to read the XML
//! and transform the data into tokens.
//!
//! First, instantiate an `Evaluator` with an input source.
//!
//! Second, if the `Evaluator` supports the iterator API, a library user may transform
//! the `Evaluator` into an iterator by calling the `into_iter()` method. Then, like other
//! iterators, you can call `next()` or use any of the other `Iterator` methods like
//! `map`, `filter`, etc. In most cases, especially if you need to further
//! transform the XML content, using the `Iterator` API is easier. The returned tokens
//! from the Iterator API have owned copies of the bytes representing the token.
//!
//! If the use case only involves reading the data and not copying or transforming the
//! data, the cursor API (usually by calling `next_token` directly on the `Evaluator` itself and not
//! transforming the `Evaluator` into an iterator) may be sufficient. The returned tokens
//! from the cursor API are representing a token by borrowing a byte slice view from
//! a shared internal buffer.
//!
//! # Example
//!
//! As a simplified and unoptimized example, the following code uses the iterator style API to
//! transform uppercase `ID` into lowercase `id` tag names.
//!
//! ```
//! # #[cfg(feature = "std")]
//! use maybe_xml::token::owned::{Token, StartTag, Characters, EndTag};
//!
//! # #[derive(Debug)]
//! # enum Error {
//! # Io(std::io::Error),
//! # Utf8(std::str::Utf8Error),
//! # }
//! # impl From<std::io::Error> for Error {
//! # fn from(e: std::io::Error) -> Self {
//! # Error::Io(e)
//! # }
//! # }
//! # impl From<std::str::Utf8Error> for Error {
//! # fn from(e: std::str::Utf8Error) -> Self {
//! # Error::Utf8(e)
//! # }
//! # }
//! # fn main() -> Result<(), Error> {
//! # #[cfg(feature = "std")]
//! # {
//! let mut input = std::io::BufReader::new(r#"<ID>Example</ID>"#.as_bytes());
//!
//! let eval = maybe_xml::eval::bufread::BufReadEvaluator::from_reader(input);
//!
//! let mut iter = eval.into_iter()
//!     .map(|token| match token {
//!         Token::StartTag(start_tag) => {
//!             if let Ok(str) = start_tag.to_str() {
//!                 Token::StartTag(StartTag::from(str.to_lowercase()))
//!             } else {
//!                 Token::StartTag(start_tag)
//!             }
//!         }
//!         Token::EndTag(end_tag) => {
//!             if let Ok(str) = end_tag.to_str() {
//!                 Token::EndTag(EndTag::from(str.to_lowercase()))
//!             } else {
//!                 Token::EndTag(end_tag)
//!             }
//!         }
//!         _ => token,
//!     });
//!
//! let token = iter.next();
//! assert_eq!(token, Some(Token::StartTag(StartTag::from("<id>"))));
//! match token {
//!     Some(Token::StartTag(start_tag)) => {
//!         assert_eq!(start_tag.name().to_str()?, "id");
//!     }
//!     _ => panic!("unexpected token"),
//! }
//! assert_eq!(iter.next(), Some(Token::Characters(Characters::from("Example"))));
//! assert_eq!(iter.next(), Some(Token::EndTag(EndTag::from("</id>"))));
//! assert_eq!(iter.next(), Some(Token::Eof));
//! assert_eq!(iter.next(), None);
//! # }
//! # Ok(())
//! # }
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
//! There are no limits on the amount of data read, so if a large input source is used,
//! an `Evaluator` will buffer a large number of bytes until the end of the current
//! token is found. For untrusted input sources such as a `std::io::BufRead`, the
//! input source could be wrapped with a type which checks the number of bytes read
//! and throws an error if too many bytes have been read.
//!
//! Another possible solution is to use a `Scanner` directly and process bytes
//! immediately instead of using an `Evaluator` which buffers the bytes until a
//! complete token is read.
//!
//! [xml]: https://www.w3.org/TR/2006/REC-xml11-20060816/

#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "alloc")]
extern crate alloc;

pub(crate) mod bytes;
pub mod eval;
pub mod scanner;
pub mod token;
