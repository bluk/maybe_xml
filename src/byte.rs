// Copyright 2023 Bryant Luk
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Lexer for byte slice.

use core::iter;

mod scanner;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenTy {
    StartTag,
    EmptyElementTag,
    EndTag,
    Characters,
    ProcessingInstruction,
    Declaration,
    Comment,
    Cdata,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token {
    pub ty: TokenTy,
    pub len: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct Lexer<'a> {
    input: &'a [u8],
}

impl<'a> Lexer<'a> {
    #[inline]
    #[must_use]
    pub fn from_slice(input: &'a [u8]) -> Self {
        Self { input }
    }

    pub fn tokenize(&self, pos: &mut usize) -> Option<Token> {
        let bytes = &self.input[*pos..];
        let token = scanner::scan(bytes)?;
        *pos += token.len;
        Some(token)
    }

    #[inline]
    pub fn iter(&self, mut pos: usize) -> impl Iterator<Item = Token> + '_ {
        iter::from_fn(move || self.tokenize(&mut pos))
    }
}

impl<'a> IntoIterator for Lexer<'a> {
    type Item = Token;

    type IntoIter = IntoIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter::new(self, 0)
    }
}

pub use scanner::scan;

// /// The returned iterator type when `into_iter()` is called on `BufReadEvaluator`.
// ///
// /// The iterator is created from a `BufReadEvaluator` by calling `into_iter()`.
// /// It returns owned tokens.
// ///
// /// One notable limitation of the iterator is that if `map()`, `filter()`, or any
// /// other iterator adapter method, then the `clear_error()` and `position()` methods
// /// are unavailable because the adapters do not have those methods.
// /// If the underlying `BufRead` encounters an error, then there is no method to
// /// clear the error state to try reading again, and the iterator will always return `None`.
// ///
// /// # Important
// ///
// /// Note that in case the underlying `BufRead` encounters an error, the iterator
// /// will store the error state and will return `None` from the `next()` method.
// ///
// /// When the end of file state is reached, `next()` will always return `None`.
// ///
// /// # Example
// ///
// /// ```
// /// # #[cfg(feature = "std")]
// /// use maybe_xml::token::owned::{Token, StartTag, Characters, EndTag};
// ///
// /// # #[derive(Debug)]
// /// # enum Error {
// /// # Io(std::io::Error),
// /// # Utf8(std::str::Utf8Error),
// /// # }
// /// # impl From<std::io::Error> for Error {
// /// # fn from(e: std::io::Error) -> Self {
// /// # Error::Io(e)
// /// # }
// /// # }
// /// # impl From<std::str::Utf8Error> for Error {
// /// # fn from(e: std::str::Utf8Error) -> Self {
// /// # Error::Utf8(e)
// /// # }
// /// # }
// /// # fn main() -> Result<(), Error> {
// /// # #[cfg(feature = "std")]
// /// # {
// /// let mut input = std::io::BufReader::new(r#"<ID>Example</ID>"#.as_bytes());
// ///
// /// let eval = maybe_xml::eval::bufread::BufReadEvaluator::from_reader(input);
// ///
// /// let mut iter = eval.into_iter()
// ///     .map(|token| match token {
// ///         Token::StartTag(start_tag) => {
// ///             if let Ok(str) = start_tag.to_str() {
// ///                 Token::StartTag(StartTag::from(str.to_lowercase()))
// ///             } else {
// ///                 Token::StartTag(start_tag)
// ///             }
// ///         }
// ///         Token::EndTag(end_tag) => {
// ///             if let Ok(str) = end_tag.to_str() {
// ///                 Token::EndTag(EndTag::from(str.to_lowercase()))
// ///             } else {
// ///                 Token::EndTag(end_tag)
// ///             }
// ///         }
// ///         _ => token,
// ///     });
// ///
// /// let token = iter.next();
// /// assert_eq!(token, Some(Token::StartTag(StartTag::from("<id>"))));
// /// match token {
// ///     Some(Token::StartTag(start_tag)) => {
// ///         assert_eq!(start_tag.name().to_str()?, "id");
// ///     }
// ///     _ => panic!("unexpected token"),
// /// }
// /// assert_eq!(iter.next(), Some(Token::Characters(Characters::from("Example"))));
// /// assert_eq!(iter.next(), Some(Token::EndTag(EndTag::from("</id>"))));
// /// assert_eq!(iter.next(), Some(Token::Eof));
// /// assert_eq!(iter.next(), None);
// /// # }
// /// # Ok(())
// /// # }
// /// ```
#[derive(Debug)]
pub struct IntoIter<'a> {
    inner: Lexer<'a>,
    pos: usize,
}

impl<'a> IntoIter<'a> {
    fn new(inner: Lexer<'a>, pos: usize) -> Self {
        Self { inner, pos }
    }
}

impl<'a> Iterator for IntoIter<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.tokenize(&mut self.pos)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn none_on_empty() {
        let lexer = Lexer::from_slice(&[]);
        let mut pos = 0;
        assert_eq!(None, lexer.tokenize(&mut pos));
    }

    #[test]
    #[should_panic(expected = "start index 1 out of range")]
    fn panic_on_pos_greater_than_slice_len() {
        let lexer = Lexer::from_slice(&[]);
        let mut pos = 1;
        lexer.tokenize(&mut pos);
    }

    #[test]
    #[should_panic(expected = "out of range")]
    fn panic_on_pos_greater_than_slice_len_2() {
        let lexer = Lexer::from_slice("hello".as_bytes());
        let mut pos = "hello".len() + 1;
        lexer.tokenize(&mut pos);
    }

    #[test]
    fn text_content() {
        let mut buf = Vec::new();
        let mut pos = 0;
        buf.extend("Hello".as_bytes());
        let lexer = Lexer::from_slice(&buf);
        assert_eq!(
            Some(Token {
                ty: TokenTy::Characters,
                len: 5
            }),
            lexer.tokenize(&mut pos)
        );
        assert_eq!(buf.len(), pos);
        buf.clear();
        pos = 0;

        buf.extend("wo".as_bytes());
        let lexer = Lexer::from_slice(&buf);
        assert_eq!(
            Some(Token {
                ty: TokenTy::Characters,
                len: 2
            }),
            lexer.tokenize(&mut pos)
        );
        assert_eq!(buf.len(), pos);
        buf.clear();
        pos = 0;

        buf.extend("rld!<".as_bytes());
        let lexer = Lexer::from_slice(&buf);
        assert_eq!(
            Some(Token {
                ty: TokenTy::Characters,
                len: 4
            }),
            lexer.tokenize(&mut pos)
        );
        assert_eq!(buf.len() - 1, pos);
    }
}
