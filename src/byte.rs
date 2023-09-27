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

    // #[test]
    // fn text_content_finish_on_empty_bytes() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"Hello".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningCharacters));
    //     assert_eq!(scanner.state, InternalState::ScanningCharacters);
    //     let bytes = r"".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedCharacters(0)));
    //     assert_eq!(scanner.state, InternalState::Eof);
    //     assert_eq!(scanner.scan(b"<hello>"), None);
    //     assert_eq!(scanner.state, InternalState::Eof);
    // }

    // #[test]
    // fn start_of_markup() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
    //     assert_eq!(scanner.state, InternalState::ScanningMarkup);
    // }

    // #[test]
    // fn start_of_markup_eof() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
    //     assert_eq!(scanner.state, InternalState::ScanningMarkup);
    //     let bytes = r"".as_bytes();
    //     assert_eq!(scanner.scan(bytes), None);
    //     assert_eq!(scanner.state, InternalState::Eof);
    //     assert_eq!(scanner.scan(b"<hello>"), None);
    //     assert_eq!(scanner.state, InternalState::Eof);
    // }

    // #[test]
    // fn start_tag_in_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<hello>".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedStartTag(7)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn start_tag_eof() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<hello".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningStartOrEmptyElementTag)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningStartOrEmptyElementTag(QuoteState::None, false)
    //     );
    //     let bytes = r"".as_bytes();
    //     assert_eq!(scanner.scan(bytes), None);
    //     assert_eq!(scanner.state, InternalState::Eof);
    //     assert_eq!(scanner.scan(b"<hello>"), None);
    //     assert_eq!(scanner.state, InternalState::Eof);
    // }

    // #[test]
    // fn start_tag_with_only_markup_in_first_part() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
    //     assert_eq!(scanner.state, InternalState::ScanningMarkup);

    //     let bytes = r"hello".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningStartOrEmptyElementTag)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningStartOrEmptyElementTag(QuoteState::None, false)
    //     );

    //     let bytes = r">Content".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedStartTag(1)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn start_tag_in_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<hello".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningStartOrEmptyElementTag)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningStartOrEmptyElementTag(QuoteState::None, false)
    //     );

    //     let bytes = r">Some content".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedStartTag(1)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn start_tag_with_single_quotes_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<hello a='val>'>Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedStartTag(16)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn start_tag_with_single_quotes_in_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<hello a='"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningStartOrEmptyElementTag)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningStartOrEmptyElementTag(QuoteState::Single, false)
    //     );

    //     let bytes = r#"val>'>Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedStartTag(6)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn start_tag_with_double_quotes_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<hello a="val>">Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedStartTag(16)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn start_tag_with_double_quotes_in_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<hello a=""#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningStartOrEmptyElementTag)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningStartOrEmptyElementTag(QuoteState::Double, false)
    //     );

    //     let bytes = r#"val>">Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedStartTag(6)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn empty_element_tag_in_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<hello/>".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedEmptyElementTag(8)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn empty_element_tag_eof() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<hello/".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningStartOrEmptyElementTag)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningStartOrEmptyElementTag(QuoteState::None, true)
    //     );
    //     let bytes = r"".as_bytes();
    //     assert_eq!(scanner.scan(bytes), None);
    //     assert_eq!(scanner.state, InternalState::Eof);
    //     assert_eq!(scanner.scan(b">"), None);
    //     assert_eq!(scanner.state, InternalState::Eof);
    // }

    // #[test]
    // fn empty_element_tag_with_slash_in_standalone_part() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
    //     assert_eq!(scanner.state, InternalState::ScanningMarkup);

    //     let bytes = r"hello".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningStartOrEmptyElementTag)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningStartOrEmptyElementTag(QuoteState::None, false)
    //     );

    //     let bytes = r"/".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningStartOrEmptyElementTag)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningStartOrEmptyElementTag(QuoteState::None, true)
    //     );

    //     let bytes = r">Content".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedEmptyElementTag(1)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn empty_element_tag_with_only_markup_in_first_part() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
    //     assert_eq!(scanner.state, InternalState::ScanningMarkup);

    //     let bytes = r"hello".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningStartOrEmptyElementTag)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningStartOrEmptyElementTag(QuoteState::None, false)
    //     );

    //     let bytes = r"/>Content".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedEmptyElementTag(2)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn empty_element_tag_with_double_quotes_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<hello a="val/>"/>Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedEmptyElementTag(18)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn empty_element_tag_with_double_quotes_in_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<hello a=""#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningStartOrEmptyElementTag)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningStartOrEmptyElementTag(QuoteState::Double, false)
    //     );

    //     let bytes = r#"val/>"/>Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedEmptyElementTag(8)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn empty_element_tag_last_slash_split_across_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<hello/"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningStartOrEmptyElementTag)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningStartOrEmptyElementTag(QuoteState::None, true)
    //     );

    //     let bytes = r#">Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedEmptyElementTag(1)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn empty_element_tag_last_slash_split_across_parts_with_single_quotes() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<hello attr='/"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningStartOrEmptyElementTag)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningStartOrEmptyElementTag(QuoteState::Single, false)
    //     );

    //     let bytes = r#">'/>Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedEmptyElementTag(4)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn empty_element_tag_last_slash_split_across_parts_with_double_quotes() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<hello attr="/"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningStartOrEmptyElementTag)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningStartOrEmptyElementTag(QuoteState::Double, false)
    //     );

    //     let bytes = r#">"/>Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedEmptyElementTag(4)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn start_tag_last_slash_split_across_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<hello/"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningStartOrEmptyElementTag)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningStartOrEmptyElementTag(QuoteState::None, true)
    //     );

    //     let bytes = r#" invalid>Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedStartTag(9)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn end_tag_in_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"</goodbye>".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedEndTag(10)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn end_tag_eof() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"</hello".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningEndTag));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningEndTag(QuoteState::None)
    //     );
    //     let bytes = r"".as_bytes();
    //     assert_eq!(scanner.scan(bytes), None);
    //     assert_eq!(scanner.state, InternalState::Eof);
    //     assert_eq!(scanner.scan(b">"), None);
    //     assert_eq!(scanner.state, InternalState::Eof);
    // }

    // #[test]
    // fn end_tag_with_only_markup_in_first_part() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
    //     assert_eq!(scanner.state, InternalState::ScanningMarkup);

    //     let bytes = r"/goodbye".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningEndTag));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningEndTag(QuoteState::None)
    //     );

    //     let bytes = r">Content".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedEndTag(1)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn end_tag_with_slash_as_only_part() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
    //     assert_eq!(scanner.state, InternalState::ScanningMarkup);

    //     let bytes = r"/".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningEndTag));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningEndTag(QuoteState::None)
    //     );

    //     let bytes = r"goodbye>Content".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedEndTag(8)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn end_tag_in_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"</goodbye".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningEndTag));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningEndTag(QuoteState::None)
    //     );

    //     let bytes = r">Some content".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedEndTag(1)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn end_tag_with_single_quotes_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"</goodbye a='val>'>Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedEndTag(19)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn end_tag_with_single_quotes_in_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"</goodbye a='"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningEndTag));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningEndTag(QuoteState::Single)
    //     );

    //     let bytes = r#"val>'>Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedEndTag(6)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn end_tag_with_double_quotes_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"</goodbye a="val>">Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedEndTag(19)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn end_tag_with_double_quotes_in_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"</goodbye a=""#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningEndTag));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningEndTag(QuoteState::Double)
    //     );

    //     let bytes = r#"val>">Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedEndTag(6)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn pi_in_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<?test a="b" ?>"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScannedProcessingInstruction(15))
    //     );
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn pi_eof() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<?test a="b" ?"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningProcessingInstruction)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningProcessingInstruction(true)
    //     );
    //     let bytes = r"".as_bytes();
    //     assert_eq!(scanner.scan(bytes), None);
    //     assert_eq!(scanner.state, InternalState::Eof);
    //     assert_eq!(scanner.scan(b">"), None);
    //     assert_eq!(scanner.state, InternalState::Eof);
    // }

    // #[test]
    // fn pi_with_only_markup_in_first_part() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
    //     assert_eq!(scanner.state, InternalState::ScanningMarkup);

    //     let bytes = r"?test".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningProcessingInstruction)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningProcessingInstruction(false)
    //     );

    //     let bytes = r"?>Content".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScannedProcessingInstruction(2))
    //     );
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn pi_with_question_mark_as_only_part() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
    //     assert_eq!(scanner.state, InternalState::ScanningMarkup);

    //     let bytes = r"?".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningProcessingInstruction)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningProcessingInstruction(false)
    //     );

    //     let bytes = r"test ?>Content".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScannedProcessingInstruction(7))
    //     );
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn pi_in_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<?test".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningProcessingInstruction)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningProcessingInstruction(false)
    //     );

    //     let bytes = r">invalid?>Some content".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScannedProcessingInstruction(10))
    //     );
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn pi_ensure_does_not_reuse() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<?".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningProcessingInstruction)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningProcessingInstruction(false)
    //     );

    //     let bytes = r">invalid?>Some content".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScannedProcessingInstruction(10))
    //     );
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn pi_with_broken_delimiter() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<?test".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningProcessingInstruction)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningProcessingInstruction(false)
    //     );

    //     let bytes = r"?".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningProcessingInstruction)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningProcessingInstruction(true)
    //     );

    //     let bytes = r#" > a="v""#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningProcessingInstruction)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningProcessingInstruction(false)
    //     );

    //     let bytes = r#"?"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningProcessingInstruction)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningProcessingInstruction(true)
    //     );

    //     let bytes = r#">"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScannedProcessingInstruction(1))
    //     );
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn pi_with_single_quotes_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<?goodbye a='val>'?>Content"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScannedProcessingInstruction(20))
    //     );
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn pi_with_single_quotes_in_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<?goodbye a='?"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningProcessingInstruction)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningProcessingInstruction(true)
    //     );

    //     let bytes = r#"val?>'?>Content"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScannedProcessingInstruction(5))
    //     );
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn pi_with_double_quotes_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<?goodbye a="val?>"?>Content"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScannedProcessingInstruction(18))
    //     );
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn pi_with_double_quotes_in_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<?goodbye a="?"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningProcessingInstruction)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningProcessingInstruction(true)
    //     );

    //     let bytes = r#"val?>"?>Content"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScannedProcessingInstruction(5))
    //     );
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn pi_not_reuse_question_mark() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<?>"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningProcessingInstruction)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningProcessingInstruction(false)
    //     );
    // }

    // #[test]
    // fn pi_not_reuse_question_mark_across_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<?"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningProcessingInstruction)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningProcessingInstruction(false)
    //     );

    //     let bytes = r#">"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningProcessingInstruction)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningProcessingInstruction(false)
    //     );
    // }

    // #[test]
    // fn declaration_in_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<!DOCTYPE test [<!ELEMENT test (#PCDATA)>]>"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScannedDeclaration(bytes.len()))
    //     );
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn declaration_eof() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<!DOCTYPE test ["#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningDeclaration(
    //             QuoteState::None,
    //             BracketCount(1),
    //             AlreadyFoundByteSeqCount(0)
    //         )
    //     );
    //     let bytes = r"".as_bytes();
    //     assert_eq!(scanner.scan(bytes), None);
    //     assert_eq!(scanner.state, InternalState::Eof);
    //     assert_eq!(scanner.scan(b">"), None);
    //     assert_eq!(scanner.state, InternalState::Eof);
    // }

    // #[test]
    // fn declaration_with_only_markup_in_first_part() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
    //     assert_eq!(scanner.state, InternalState::ScanningMarkup);

    //     let bytes = r"!ELEMENT".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningDeclaration(
    //             QuoteState::None,
    //             BracketCount(0),
    //             AlreadyFoundByteSeqCount(0)
    //         )
    //     );

    //     let bytes = r">Content".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(1)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn declaration_with_exclamation_as_only_part() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
    //     assert_eq!(scanner.state, InternalState::ScanningMarkup);

    //     let bytes = r"!".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningDeclarationCommentOrCdata)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningDeclarationCommentOrCdata([0; 7], 0)
    //     );

    //     let bytes = r"test >Content".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(6)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn declaration_in_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<!test".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningDeclaration(
    //             QuoteState::None,
    //             BracketCount(0),
    //             AlreadyFoundByteSeqCount(0)
    //         )
    //     );

    //     let bytes = r">Some content".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(1)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn declaration_with_single_quotes_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<!goodbye a='val>'>Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(19)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn declaration_with_single_quotes_in_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<!goodbye a='>"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningDeclaration(
    //             QuoteState::Single,
    //             BracketCount(0),
    //             AlreadyFoundByteSeqCount(0)
    //         )
    //     );

    //     let bytes = r#"val>'>Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(6)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn declaration_with_double_quotes_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<!goodbye a="val>">Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(19)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn declaration_with_double_quotes_in_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<!goodbye a=">"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningDeclaration(
    //             QuoteState::Double,
    //             BracketCount(0),
    //             AlreadyFoundByteSeqCount(0)
    //         )
    //     );

    //     let bytes = r#"val>">Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(6)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn declaration_with_closed_brackets() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<![%test;[<!ELEMENT test (something*)>]]>"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScannedDeclaration(bytes.len()))
    //     );
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn declaration_with_unclosed_single_bracket() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<![test>"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningDeclaration(
    //             QuoteState::None,
    //             BracketCount(1),
    //             AlreadyFoundByteSeqCount(0)
    //         )
    //     );

    //     let bytes = r#">] >Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(4)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn declaration_with_unclosed_double_bracket() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<![test>"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningDeclaration(
    //             QuoteState::None,
    //             BracketCount(1),
    //             AlreadyFoundByteSeqCount(0)
    //         )
    //     );

    //     let bytes = r#"[more"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningDeclaration(
    //             QuoteState::None,
    //             BracketCount(2),
    //             AlreadyFoundByteSeqCount(0)
    //         )
    //     );

    //     let bytes = r#">] >Content>"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningDeclaration(
    //             QuoteState::None,
    //             BracketCount(1),
    //             AlreadyFoundByteSeqCount(0)
    //         )
    //     );

    //     let bytes = r#">] >Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(4)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn comment_in_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<!-- Comment -->"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScannedComment(bytes.len()))
    //     );
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn comment_eof() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<!-- Comment"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningComment(AlreadyFoundByteSeqCount(0))
    //     );
    //     let bytes = r"".as_bytes();
    //     assert_eq!(scanner.scan(bytes), None);
    //     assert_eq!(scanner.state, InternalState::Eof);
    //     assert_eq!(scanner.scan(b">"), None);
    //     assert_eq!(scanner.state, InternalState::Eof);
    // }

    // #[test]
    // fn comment_with_only_markup_in_first_part() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
    //     assert_eq!(scanner.state, InternalState::ScanningMarkup);

    //     let bytes = r"!--".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningComment(AlreadyFoundByteSeqCount(0))
    //     );

    //     let bytes = r" Comment --> Content".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(12)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn comment_with_exclamation_as_only_part() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
    //     assert_eq!(scanner.state, InternalState::ScanningMarkup);

    //     let bytes = r"!".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningDeclarationCommentOrCdata)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningDeclarationCommentOrCdata([0; 7], 0)
    //     );

    //     let bytes = r"-- Comment -->Content".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(14)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn comment_in_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<!-- test".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningComment(AlreadyFoundByteSeqCount(0))
    //     );

    //     let bytes = r" -->Some content".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(4)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn comment_with_single_quotes_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<!-- goodbye a='val-->'-->Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(22)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn comment_with_single_quotes_in_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<!--goodbye a='--"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningComment(AlreadyFoundByteSeqCount(2))
    //     );

    //     let bytes = r#"val>'-->Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(8)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn comment_with_double_quotes_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<!--goodbye a="val-->"-->Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(21)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn comment_with_double_quotes_in_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<!--goodbye a="--"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningComment(AlreadyFoundByteSeqCount(2))
    //     );

    //     let bytes = r#"val-->"-->Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(6)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn comment_with_invalid_start() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<!-goodbye a="-->"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningDeclaration(
    //             QuoteState::Double,
    //             BracketCount(0),
    //             AlreadyFoundByteSeqCount(0)
    //         )
    //     );

    //     let bytes = r#"val-->">Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(8)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn comment_with_double_dash_inside() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<!--goodbye a="--"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningComment(AlreadyFoundByteSeqCount(2))
    //     );

    //     let bytes = r#"val-->"-- test -->Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(6)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn comment_with_single_dash() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<!--goodbye a="--"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningComment(AlreadyFoundByteSeqCount(2))
    //     );

    //     let bytes = r#"val--" test ->Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningComment(AlreadyFoundByteSeqCount(0))
    //     );

    //     let bytes = r#"More -->Real Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(8)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn comment_with_split_terminating_delimiter() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<!--goodbye a="--"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningComment(AlreadyFoundByteSeqCount(2))
    //     );

    //     let bytes = r#"val->" -"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningComment(AlreadyFoundByteSeqCount(1))
    //     );

    //     let bytes = r#"-"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningComment(AlreadyFoundByteSeqCount(2))
    //     );

    //     let bytes = r#">"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(1)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn comment_not_reused_dashes() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<!-->"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningComment(AlreadyFoundByteSeqCount(0))
    //     );

    //     let bytes = r#"-->"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(3)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn comment_not_reused_dashes_across_scans() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<!--"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningComment(AlreadyFoundByteSeqCount(0))
    //     );

    //     let bytes = r#">"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningComment(AlreadyFoundByteSeqCount(0))
    //     );
    // }

    // #[test]
    // fn cdata_in_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<![CDATA[ Content ]]>"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(bytes.len())));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn declaration_with_uneven_brackets_in_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<![&random[ Declaration ]]]>"#.as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScannedDeclaration(bytes.len()))
    //     );
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn cdata_eof() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<![CDATA[ Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningCdata));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningCdata(AlreadyFoundByteSeqCount(0))
    //     );
    //     let bytes = r"".as_bytes();
    //     assert_eq!(scanner.scan(bytes), None);
    //     assert_eq!(scanner.state, InternalState::Eof);
    //     assert_eq!(scanner.scan(b"]]>"), None);
    //     assert_eq!(scanner.state, InternalState::Eof);
    // }

    // #[test]
    // fn cdata_with_only_markup_in_first_part() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
    //     assert_eq!(scanner.state, InternalState::ScanningMarkup);

    //     let bytes = r"![CDAT".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningDeclarationCommentOrCdata)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningDeclarationCommentOrCdata(
    //             [b'[', b'C', b'D', b'A', b'T', 0, 0],
    //             5
    //         )
    //     );

    //     let bytes = r"A[ Content ]]> Unused Content".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(14)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn cdata_with_exclamation_as_only_part() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
    //     assert_eq!(scanner.state, InternalState::ScanningMarkup);

    //     let bytes = r"!".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningDeclarationCommentOrCdata)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningDeclarationCommentOrCdata([0, 0, 0, 0, 0, 0, 0], 0)
    //     );

    //     let bytes = r"[CDATA[ Content ]]>Content".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(19)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn cdata_in_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r"<![CDA".as_bytes();
    //     assert_eq!(
    //         scanner.scan(bytes),
    //         Some(State::ScanningDeclarationCommentOrCdata)
    //     );
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningDeclarationCommentOrCdata([b'[', b'C', b'D', b'A', 0, 0, 0], 4)
    //     );

    //     let bytes = r"TA[ Content ]]>Some content".as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(15)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn cdata_with_single_quotes_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<![CDATA[ Content ']]>']]>Unused Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(22)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn cdata_with_single_quotes_in_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<![CDATA[ ']>"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningCdata));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningCdata(AlreadyFoundByteSeqCount(0))
    //     );

    //     let bytes = r#"]>']]>Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(6)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn cdata_with_double_quotes_one_pass() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<![CDATA[ goodbye a="]]>"]]>Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(24)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn cdata_with_double_quotes_in_parts() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<![CDATA[ a="]>"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningCdata));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningCdata(AlreadyFoundByteSeqCount(0))
    //     );

    //     let bytes = r#"]>"]]>Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(6)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn cdata_with_invalid_start() {
    //     let mut scanner = Scanner::new();
    //     // missing "["
    //     let bytes = r#"<![CDATA Content a="]]>"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningDeclaration(
    //             QuoteState::Double,
    //             BracketCount(1),
    //             AlreadyFoundByteSeqCount(0)
    //         )
    //     );

    //     let bytes = r#"]]>"]]>Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(7)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn cdata_with_double_right_bracket_inside() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<![CDATA[ Content a="]>"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningCdata));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningCdata(AlreadyFoundByteSeqCount(0))
    //     );

    //     let bytes = r#"other ]]"]] test ]]>Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(20)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn cdata_with_single_closing_bracket() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<![CDATA[ Content a="]>"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningCdata));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningCdata(AlreadyFoundByteSeqCount(0))
    //     );

    //     let bytes = r#"]>" test ]>Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningCdata));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningCdata(AlreadyFoundByteSeqCount(0))
    //     );

    //     let bytes = r#"More ]]>Real Content"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(8)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }

    // #[test]
    // fn cdata_with_split_terminating_delimiter() {
    //     let mut scanner = Scanner::new();
    //     let bytes = r#"<![CDATA[ goodbye a="]>"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningCdata));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningCdata(AlreadyFoundByteSeqCount(0))
    //     );

    //     let bytes = r#"val]>" ]"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningCdata));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningCdata(AlreadyFoundByteSeqCount(1))
    //     );

    //     let bytes = r#"]"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScanningCdata));
    //     assert_eq!(
    //         scanner.state,
    //         InternalState::ScanningCdata(AlreadyFoundByteSeqCount(2))
    //     );

    //     let bytes = r#">"#.as_bytes();
    //     assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(1)));
    //     assert_eq!(scanner.state, InternalState::Reset);
    // }
}
