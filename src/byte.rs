// Copyright 2023 Bryant Luk
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Lexer for byte slice.

use core::iter;

use crate::token::borrowed::TokenTy;

mod scanner;

pub use scanner::scan;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token<'a> {
    pub ty: TokenTy<'a>,
    pub offset: usize,
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

    pub fn tokenize(&self, pos: &mut usize) -> Option<Token<'a>> {
        let bytes = &self.input[*pos..];
        let mut token = scan(bytes)?;
        token.offset = *pos;
        *pos += token.len;
        Some(token)
    }

    #[inline]
    pub fn iter(&self, mut pos: usize) -> impl Iterator<Item = Token<'a>> + '_ {
        iter::from_fn(move || self.tokenize(&mut pos))
    }
}

impl<'a> IntoIterator for Lexer<'a> {
    type Item = Token<'a>;

    type IntoIter = IntoIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter::new(self, 0)
    }
}

/// The returned iterator type when [`IntoIterator::into_iter()`] is called on [`Lexer`].
///
/// # Example
///
/// ```
/// use maybe_xml::{byte::{Lexer}, token::borrowed::{EndTag, StartTag, TokenTy}};
/// use std::io::BufRead;
///
/// let mut input = std::io::BufReader::new(r#"<ID>Example</id><name>Jane Doe</name>"#.as_bytes());
/// let buffer = input.fill_buf()?;
/// let lexer = Lexer::from_slice(buffer);
///
/// let mut iter = lexer.into_iter()
///     .filter_map(|token| {
///         match token.ty {
///             TokenTy::StartTag(start_tag) => {
///                 Some(start_tag.name().to_str())
///             }
///             TokenTy::EndTag(end_tag) => {
///                 Some(end_tag.name().to_str())
///             }
///             _ => None,
///         }
///     });
///
/// assert_eq!(Some(Ok("ID")), iter.next());
/// assert_eq!(Some(Ok("id")), iter.next());
/// assert_eq!(Some(Ok("name")), iter.next());
/// assert_eq!(Some(Ok("name")), iter.next());
/// assert_eq!(None, iter.next());
/// # Ok::<(), std::io::Error>(())
/// ```
#[derive(Debug)]
pub struct IntoIter<'a> {
    inner: Lexer<'a>,
    pos: usize,
}

impl<'a> IntoIter<'a> {
    #[inline]
    #[must_use]
    pub fn new(inner: Lexer<'a>, pos: usize) -> Self {
        Self { inner, pos }
    }
}

impl<'a> Iterator for IntoIter<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.tokenize(&mut self.pos)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(all(feature = "alloc", not(feature = "std")))]
    use alloc::vec::Vec;
    #[cfg(feature = "std")]
    use std::vec::Vec;

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

    #[cfg(any(feature = "std", feature = "alloc"))]
    #[test]
    fn text_content() {
        use crate::token::borrowed::Characters;

        let mut buf = Vec::new();
        let mut pos = 0;
        buf.extend("Hello".as_bytes());
        let lexer = Lexer::from_slice(&buf);
        assert_eq!(
            Some(Token {
                ty: TokenTy::Characters(Characters::from("Hello".as_bytes())),
                offset: 0,
                len: 5
            }),
            lexer.tokenize(&mut pos)
        );
        assert_eq!(buf.len(), pos);

        buf.extend("wo".as_bytes());
        let lexer = Lexer::from_slice(&buf);
        assert_eq!(
            Some(Token {
                ty: TokenTy::Characters(Characters::from("wo".as_bytes())),
                offset: 5,
                len: 2
            }),
            lexer.tokenize(&mut pos)
        );
        assert_eq!(buf.len(), pos);

        buf.extend("rld!<".as_bytes());
        let lexer = Lexer::from_slice(&buf);
        assert_eq!(
            Some(Token {
                ty: TokenTy::Characters(Characters::from("rld!".as_bytes())),
                offset: 7,
                len: 4
            }),
            lexer.tokenize(&mut pos)
        );
        assert_eq!(buf.len() - 1, pos);
    }
}
