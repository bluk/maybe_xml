// Copyright 2020 Bryant Luk
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Provides an iterator and cursor API for `BufRead` input sources.

#[cfg(any(feature = "std"))]
use crate::scanner::{Scanner, State};
#[cfg(feature = "std")]
use crate::token::owned::{
    BytesNotEvaluated, Cdata, Characters, Comment, Declaration, EmptyElementTag, EndTag,
    ProcessingInstruction, StartTag, Token,
};
#[cfg(any(feature = "std"))]
use core::convert::TryFrom;

#[cfg(feature = "std")]
use std::io::{BufRead, Error};

/// Reads and buffers bytes from a `BufRead` input source and returns completed tokens.
///
/// Provides both an iterator and cursor API to read the contents.
///
/// The evaluator can be turned into an iterator by calling `into_iter()`. The iterator
/// will return owned tokens until the "end of file" is reached. If the end of file is reached,
/// the iterator's next method will always return `None`.
///
/// The cursor API is provided with the `next_token()` method which returns borrowed
/// tokens. The tokens borrow from an internal shared buffer.
///
/// # Example
///
/// ```
/// # #[cfg(feature = "std")]
/// use maybe_xml::token::borrowed::{Token, StartTag, Characters, EndTag};
///
/// # #[derive(Debug)]
/// # enum Error {
/// # Io(std::io::Error),
/// # Utf8(std::str::Utf8Error),
/// # }
/// # impl From<std::io::Error> for Error {
/// # fn from(e: std::io::Error) -> Self {
/// # Error::Io(e)
/// # }
/// # }
/// # impl From<std::str::Utf8Error> for Error {
/// # fn from(e: std::str::Utf8Error) -> Self {
/// # Error::Utf8(e)
/// # }
/// # }
/// # fn main() -> Result<(), Error> {
/// # #[cfg(feature = "std")]
/// # {
/// let mut input = std::io::BufReader::new(r#"<id>Example</id>"#.as_bytes());
///
/// let mut eval = maybe_xml::eval::bufread::BufReadEvaluator::from_reader(input);
///
/// let token = eval.next_token()?;
/// assert_eq!(token, Some(Token::StartTag(StartTag::from("<id>"))));
/// match token {
///     Some(Token::StartTag(start_tag)) => {
///         assert_eq!(start_tag.name().to_str()?, "id");
///     }
///     _ => panic!("unexpected token"),
/// }
/// assert_eq!(eval.next_token()?, Some(Token::Characters(Characters::from("Example"))));
/// assert_eq!(eval.next_token()?, Some(Token::EndTag(EndTag::from("</id>"))));
/// assert_eq!(eval.next_token()?, Some(Token::Eof));
/// assert_eq!(eval.next_token()?, None);
/// # }
/// # Ok(())
/// # }
/// ```
#[derive(Clone, Debug)]
#[cfg(feature = "std")]
pub struct BufReadEvaluator<B>
where
    B: BufRead,
{
    reader: B,
    scanner: Scanner,
    position: u64,
    buffer: Vec<u8>,
    is_eof: bool,
}

#[cfg(feature = "std")]
impl<B> BufReadEvaluator<B>
where
    B: BufRead,
{
    /// Returns a `BufReadEvaluator` from a `BufRead` instance.
    pub fn from_reader(reader: B) -> Self {
        Self {
            reader,
            scanner: Scanner::new(),
            position: 0,
            buffer: Vec::new(),
            is_eof: false,
        }
    }

    /// Returns the current index position if the `BufRead` was a continuous byte sequence.
    ///
    /// The position is mostly useful for debugging.
    pub fn position(&self) -> u64 {
        self.position
    }

    /// Returns the underlying `BufRead`.
    pub fn into_inner(self) -> B {
        self.reader
    }

    /// Reads the next token using an internal shared buffer.
    ///
    /// # Important
    ///
    /// Note that once the end of file is reached, the method will always return `Ok(None)`.
    ///
    /// # Errors
    ///
    /// `std::io::Error` may be returned from the method if the underlying `BufRead` errors
    /// on any of its method calls.
    pub fn next_token(&mut self) -> Result<Option<crate::token::borrowed::Token<'_>>, Error> {
        use crate::token::borrowed::{
            BytesNotEvaluated, Cdata, Characters, Comment, Declaration, EmptyElementTag, EndTag,
            ProcessingInstruction, StartTag, Token,
        };

        if self.is_eof {
            return Ok(None);
        }

        self.buffer.clear();

        loop {
            let bytes = self.reader.fill_buf()?;
            let state = self.scanner.scan(bytes);
            match state {
                None => {
                    self.is_eof = true;
                    if self.buffer.is_empty() {
                        return Ok(Some(Token::Eof));
                    } else {
                        return Ok(Some(Token::EofWithBytesNotEvaluated(
                            BytesNotEvaluated::from(&self.buffer),
                        )));
                    }
                }
                Some(State::ScanningMarkup) => {
                    self.buffer.extend_from_slice(bytes);
                    let bytes_len = bytes.len();
                    self.position += u64::try_from(bytes_len)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                    self.reader.consume(bytes_len);
                }
                Some(State::ScanningStartOrEmptyElementTag) => {
                    self.buffer.extend_from_slice(bytes);
                    let bytes_len = bytes.len();
                    self.position += u64::try_from(bytes_len)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                    self.reader.consume(bytes_len);
                }
                Some(State::ScanningCharacters) => {
                    self.buffer.extend_from_slice(bytes);
                    let bytes_len = bytes.len();
                    self.position += u64::try_from(bytes_len)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                    self.reader.consume(bytes_len);
                }
                Some(State::ScanningEndTag) => {
                    self.buffer.extend_from_slice(bytes);
                    let bytes_len = bytes.len();
                    self.position += u64::try_from(bytes_len)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                    self.reader.consume(bytes_len);
                }
                Some(State::ScanningProcessingInstruction) => {
                    self.buffer.extend_from_slice(bytes);
                    let bytes_len = bytes.len();
                    self.position += u64::try_from(bytes_len)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                    self.reader.consume(bytes_len);
                }
                Some(State::ScanningDeclarationCommentOrCdata) => {
                    self.buffer.extend_from_slice(bytes);
                    let bytes_len = bytes.len();
                    self.position += u64::try_from(bytes_len)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                    self.reader.consume(bytes_len);
                }
                Some(State::ScanningDeclaration) => {
                    self.buffer.extend_from_slice(bytes);
                    let bytes_len = bytes.len();
                    self.position += u64::try_from(bytes_len)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                    self.reader.consume(bytes_len);
                }
                Some(State::ScanningComment) => {
                    self.buffer.extend_from_slice(bytes);
                    let bytes_len = bytes.len();
                    self.position += u64::try_from(bytes_len)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                    self.reader.consume(bytes_len);
                }
                Some(State::ScanningCdata) => {
                    self.buffer.extend_from_slice(bytes);
                    let bytes_len = bytes.len();
                    self.position += u64::try_from(bytes_len)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                    self.reader.consume(bytes_len);
                }
                Some(State::ScannedStartTag(read)) => {
                    self.buffer.extend_from_slice(&bytes[..read]);
                    self.position += u64::try_from(read)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                    self.reader.consume(read);
                    return Ok(Some(Token::StartTag(StartTag::from(&self.buffer))));
                }
                Some(State::ScannedEmptyElementTag(read)) => {
                    self.buffer.extend_from_slice(&bytes[..read]);
                    self.position += u64::try_from(read)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                    self.reader.consume(read);
                    return Ok(Some(Token::EmptyElementTag(EmptyElementTag::from(
                        &self.buffer,
                    ))));
                }
                Some(State::ScannedEndTag(read)) => {
                    self.buffer.extend_from_slice(&bytes[..read]);
                    self.position += u64::try_from(read)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                    self.reader.consume(read);
                    return Ok(Some(Token::EndTag(EndTag::from(&self.buffer))));
                }
                Some(State::ScannedCharacters(read)) => {
                    self.buffer.extend_from_slice(&bytes[..read]);
                    self.position += u64::try_from(read)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                    self.reader.consume(read);
                    return Ok(Some(Token::Characters(Characters::from(&self.buffer))));
                }
                Some(State::ScannedProcessingInstruction(read)) => {
                    self.buffer.extend_from_slice(&bytes[..read]);
                    self.position += u64::try_from(read)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                    self.reader.consume(read);
                    return Ok(Some(Token::ProcessingInstruction(
                        ProcessingInstruction::from(&self.buffer),
                    )));
                }
                Some(State::ScannedDeclaration(read)) => {
                    self.buffer.extend_from_slice(&bytes[..read]);
                    self.position += u64::try_from(read)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                    self.reader.consume(read);
                    return Ok(Some(Token::Declaration(Declaration::from(&self.buffer))));
                }
                Some(State::ScannedComment(read)) => {
                    self.buffer.extend_from_slice(&bytes[..read]);
                    self.position += u64::try_from(read)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                    self.reader.consume(read);
                    return Ok(Some(Token::Comment(Comment::from(&self.buffer))));
                }
                Some(State::ScannedCdata(read)) => {
                    self.buffer.extend_from_slice(&bytes[..read]);
                    self.position += u64::try_from(read)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                    self.reader.consume(read);
                    return Ok(Some(Token::Cdata(Cdata::from(&self.buffer))));
                }
            };
        }
    }
}

#[cfg(feature = "std")]
impl<B> IntoIterator for BufReadEvaluator<B>
where
    B: BufRead,
{
    type Item = Token;
    type IntoIter = IntoIter<B>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter::new(self)
    }
}

/// The returned iterator type when `into_iter()` is called on `BufReadEvaluator`.
///
/// The iterator is created from a `BufReadEvaluator` by calling `into_iter()`.
/// It returns owned tokens.
///
/// One notable limitation of the iterator is that if `map()`, `filter()`, or any
/// other iterator adapter method, then the `clear_error()` and `position()` methods
/// are unavailable because the adapters do not have those methods.
/// If the underlying `BufRead` encounters an error, then there is no method to
/// clear the error state to try reading again, and the iterator will always return `None`.
///
/// # Important
///
/// Note that in case the underlying `BufRead` encounters an error, the iterator
/// will store the error state and will return `None` from the `next()` method.
///
/// When the end of file state is reached, `next()` will always return `None`.
///
/// # Example
///
/// ```
/// # #[cfg(feature = "std")]
/// use maybe_xml::token::owned::{Token, StartTag, Characters, EndTag};
///
/// # #[derive(Debug)]
/// # enum Error {
/// # Io(std::io::Error),
/// # Utf8(std::str::Utf8Error),
/// # }
/// # impl From<std::io::Error> for Error {
/// # fn from(e: std::io::Error) -> Self {
/// # Error::Io(e)
/// # }
/// # }
/// # impl From<std::str::Utf8Error> for Error {
/// # fn from(e: std::str::Utf8Error) -> Self {
/// # Error::Utf8(e)
/// # }
/// # }
/// # fn main() -> Result<(), Error> {
/// # #[cfg(feature = "std")]
/// # {
/// let mut input = std::io::BufReader::new(r#"<ID>Example</ID>"#.as_bytes());
///
/// let eval = maybe_xml::eval::bufread::BufReadEvaluator::from_reader(input);
///
/// let mut iter = eval.into_iter()
///     .map(|token| match token {
///         Token::StartTag(start_tag) => {
///             if let Ok(str) = start_tag.to_str() {
///                 Token::StartTag(StartTag::from(str.to_lowercase()))
///             } else {
///                 Token::StartTag(start_tag)
///             }
///         }
///         Token::EndTag(end_tag) => {
///             if let Ok(str) = end_tag.to_str() {
///                 Token::EndTag(EndTag::from(str.to_lowercase()))
///             } else {
///                 Token::EndTag(end_tag)
///             }
///         }
///         _ => token,
///     });
///
/// let token = iter.next();
/// assert_eq!(token, Some(Token::StartTag(StartTag::from("<id>"))));
/// match token {
///     Some(Token::StartTag(start_tag)) => {
///         assert_eq!(start_tag.name().to_str()?, "id");
///     }
///     _ => panic!("unexpected token"),
/// }
/// assert_eq!(iter.next(), Some(Token::Characters(Characters::from("Example"))));
/// assert_eq!(iter.next(), Some(Token::EndTag(EndTag::from("</id>"))));
/// assert_eq!(iter.next(), Some(Token::Eof));
/// assert_eq!(iter.next(), None);
/// # }
/// # Ok(())
/// # }
/// ```
#[cfg(feature = "std")]
#[derive(Debug)]
pub struct IntoIter<B>
where
    B: BufRead,
{
    reader: BufReadEvaluator<B>,
    error: Option<std::io::Error>,
}

#[cfg(feature = "std")]
impl<B> IntoIter<B>
where
    B: BufRead,
{
    fn new(reader: BufReadEvaluator<B>) -> Self {
        Self {
            reader,
            error: None,
        }
    }

    /// Returns the current index position if the `BufRead` was a continuous byte sequence.
    ///
    /// The position is mostly useful for debugging.
    pub fn position(&self) -> u64 {
        self.reader.position()
    }

    /// Clears any error state and returns the error if it exists.
    pub fn clear_error(&mut self) -> Option<std::io::Error> {
        self.error.take()
    }
}

#[cfg(feature = "std")]
impl<B> Iterator for IntoIter<B>
where
    B: BufRead,
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.reader.is_eof || self.error.is_some() {
            return None;
        }

        let mut buf = Vec::new();
        loop {
            let bytes = match self.reader.reader.fill_buf() {
                Ok(bytes) => bytes,
                Err(e) => {
                    self.error = Some(e);
                    return None;
                }
            };
            let state = self.reader.scanner.scan(bytes);
            match state {
                None => {
                    self.reader.is_eof = true;
                    if buf.is_empty() {
                        return Some(Token::Eof);
                    } else {
                        return Some(Token::EofWithBytesNotEvaluated(BytesNotEvaluated::from(
                            buf,
                        )));
                    }
                }
                Some(State::ScanningMarkup) => {
                    buf.extend_from_slice(bytes);
                    let bytes_len = bytes.len();
                    self.reader.position += u64::try_from(bytes_len)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                    self.reader.reader.consume(bytes_len);
                }
                Some(State::ScanningStartOrEmptyElementTag) => {
                    buf.extend_from_slice(bytes);
                    let bytes_len = bytes.len();
                    self.reader.position += u64::try_from(bytes_len)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                    self.reader.reader.consume(bytes_len);
                }
                Some(State::ScanningCharacters) => {
                    buf.extend_from_slice(bytes);
                    let bytes_len = bytes.len();
                    self.reader.position += u64::try_from(bytes_len)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                    self.reader.reader.consume(bytes_len);
                }
                Some(State::ScanningEndTag) => {
                    buf.extend_from_slice(bytes);
                    let bytes_len = bytes.len();
                    self.reader.position += u64::try_from(bytes_len)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                    self.reader.reader.consume(bytes_len);
                }
                Some(State::ScanningProcessingInstruction) => {
                    buf.extend_from_slice(bytes);
                    let bytes_len = bytes.len();
                    self.reader.position += u64::try_from(bytes_len)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                    self.reader.reader.consume(bytes_len);
                }
                Some(State::ScanningDeclarationCommentOrCdata) => {
                    buf.extend_from_slice(bytes);
                    let bytes_len = bytes.len();
                    self.reader.position += u64::try_from(bytes_len)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                    self.reader.reader.consume(bytes_len);
                }
                Some(State::ScanningDeclaration) => {
                    buf.extend_from_slice(bytes);
                    let bytes_len = bytes.len();
                    self.reader.position += u64::try_from(bytes_len)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                    self.reader.reader.consume(bytes_len);
                }
                Some(State::ScanningComment) => {
                    buf.extend_from_slice(bytes);
                    let bytes_len = bytes.len();
                    self.reader.position += u64::try_from(bytes_len)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                    self.reader.reader.consume(bytes_len);
                }
                Some(State::ScanningCdata) => {
                    buf.extend_from_slice(bytes);
                    let bytes_len = bytes.len();
                    self.reader.position += u64::try_from(bytes_len)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                    self.reader.reader.consume(bytes_len);
                }
                Some(State::ScannedStartTag(read)) => {
                    buf.extend_from_slice(&bytes[..read]);
                    self.reader.position += u64::try_from(read)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                    self.reader.reader.consume(read);
                    return Some(Token::StartTag(StartTag::from(buf)));
                }
                Some(State::ScannedEmptyElementTag(read)) => {
                    buf.extend_from_slice(&bytes[..read]);
                    self.reader.position += u64::try_from(read)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                    self.reader.reader.consume(read);
                    return Some(Token::EmptyElementTag(EmptyElementTag::from(buf)));
                }
                Some(State::ScannedEndTag(read)) => {
                    buf.extend_from_slice(&bytes[..read]);
                    self.reader.position += u64::try_from(read)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                    self.reader.reader.consume(read);
                    return Some(Token::EndTag(EndTag::from(buf)));
                }
                Some(State::ScannedCharacters(read)) => {
                    buf.extend_from_slice(&bytes[..read]);
                    self.reader.position += u64::try_from(read)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                    self.reader.reader.consume(read);
                    return Some(Token::Characters(Characters::from(buf)));
                }
                Some(State::ScannedProcessingInstruction(read)) => {
                    buf.extend_from_slice(&bytes[..read]);
                    self.reader.position += u64::try_from(read)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                    self.reader.reader.consume(read);
                    return Some(Token::ProcessingInstruction(ProcessingInstruction::from(
                        buf,
                    )));
                }
                Some(State::ScannedDeclaration(read)) => {
                    buf.extend_from_slice(&bytes[..read]);
                    self.reader.position += u64::try_from(read)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                    self.reader.reader.consume(read);
                    return Some(Token::Declaration(Declaration::from(buf)));
                }
                Some(State::ScannedComment(read)) => {
                    buf.extend_from_slice(&bytes[..read]);
                    self.reader.position += u64::try_from(read)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                    self.reader.reader.consume(read);
                    return Some(Token::Comment(Comment::from(buf)));
                }
                Some(State::ScannedCdata(read)) => {
                    buf.extend_from_slice(&bytes[..read]);
                    self.reader.position += u64::try_from(read)
                        .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                    self.reader.reader.consume(read);
                    return Some(Token::Cdata(Cdata::from(buf)));
                }
            };
        }
    }
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use super::*;
    use crate::token::borrowed::{
        BytesNotEvaluated, Cdata, Characters, Comment, Declaration, EmptyElementTag, EndTag,
        ProcessingInstruction, StartTag, Token,
    };
    use rand::{thread_rng, Rng};

    type Result<T> = core::result::Result<T, Error>;

    struct TestBufRead {
        bytes: Vec<u8>,
        index: usize,
        fill_size: usize,
    }

    impl TestBufRead {
        fn with_bytes<B: AsRef<[u8]>>(bytes: B) -> Self {
            Self {
                bytes: bytes.as_ref().to_vec(),
                index: 0,
                fill_size: 0,
            }
        }

        fn rand_fill_size(&mut self) {
            if self.index + self.fill_size < self.bytes.len() {
                let mut rng = thread_rng();
                self.fill_size +=
                    rng.gen_range(1, 2 + self.bytes.len() - self.index - self.fill_size);
            }
        }
    }

    impl std::io::Read for TestBufRead {
        fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
            let read = usize::min(buf.len(), self.bytes.len() - self.index);
            buf.copy_from_slice(&self.bytes[self.index..read]);
            self.index += read;
            Ok(read)
        }
    }

    impl BufRead for TestBufRead {
        fn fill_buf(&mut self) -> Result<&[u8]> {
            self.rand_fill_size();

            let end = usize::min(self.index + self.fill_size, self.bytes.len());
            Ok(&self.bytes[self.index..end])
        }

        fn consume(&mut self, amt: usize) {
            self.fill_size -= amt;
            self.index += amt;
        }
    }

    #[test]
    fn empty() -> Result<()> {
        let xml = r"";
        let mut eval = BufReadEvaluator::from_reader(xml.as_bytes());
        assert_eq!(eval.next_token()?, Some(Token::Eof));
        assert_eq!(eval.position(), 0);
        assert_eq!(eval.next_token()?, None);
        assert_eq!(eval.position(), 0);
        Ok(())
    }

    #[test]
    fn characters_content() -> Result<()> {
        let xml = r"hello world";
        let bufreader = TestBufRead::with_bytes(xml);
        let mut eval = BufReadEvaluator::from_reader(bufreader);
        let text_content = Characters::from(b"hello world".as_ref());
        assert_eq!(eval.next_token()?, Some(Token::Characters(text_content)));
        assert_eq!(eval.position(), 11);
        assert_eq!(eval.next_token()?, Some(Token::Eof));
        assert_eq!(eval.position(), 11);
        assert_eq!(eval.next_token()?, None);
        assert_eq!(eval.position(), 11);
        Ok(())
    }

    #[test]
    fn start_tag() -> Result<()> {
        let xml = r"<hello>";
        let bufreader = TestBufRead::with_bytes(xml);
        let mut eval = BufReadEvaluator::from_reader(bufreader);
        let start_tag = StartTag::from(b"<hello>".as_ref());
        assert_eq!(eval.next_token()?, Some(Token::StartTag(start_tag)));
        assert_eq!(eval.position(), 7);
        assert_eq!(eval.next_token()?, Some(Token::Eof));
        assert_eq!(eval.position(), 7);
        assert_eq!(eval.next_token()?, None);
        assert_eq!(eval.position(), 7);
        Ok(())
    }

    #[test]
    fn start_tag_with_double_quote_attribute() -> Result<()> {
        let xml = r#"<hello name="rust">"#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut eval = BufReadEvaluator::from_reader(bufreader);
        let start_tag = StartTag::from(r#"<hello name="rust">"#.as_bytes());
        assert_eq!(eval.next_token()?, Some(Token::StartTag(start_tag)));
        assert_eq!(eval.position(), 19);
        assert_eq!(eval.next_token()?, Some(Token::Eof));
        assert_eq!(eval.position(), 19);
        assert_eq!(eval.next_token()?, None);
        assert_eq!(eval.position(), 19);
        Ok(())
    }

    #[test]
    fn start_tag_with_double_quote_attribute_with_angle_bracket() -> Result<()> {
        let xml = r#"<hello name="ru>st">"#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut eval = BufReadEvaluator::from_reader(bufreader);
        let start_tag = StartTag::from(r#"<hello name="ru>st">"#.as_bytes());
        assert_eq!(eval.next_token()?, Some(Token::StartTag(start_tag)));
        assert_eq!(eval.position(), 20);
        assert_eq!(eval.next_token()?, Some(Token::Eof));
        assert_eq!(eval.position(), 20);
        assert_eq!(eval.next_token()?, None);
        assert_eq!(eval.position(), 20);
        Ok(())
    }

    #[test]
    fn start_tag_with_single_quote_attribute() -> Result<()> {
        let xml = r#"<hello name='rust'>"#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut eval = BufReadEvaluator::from_reader(bufreader);
        let start_tag = StartTag::from(r#"<hello name='rust'>"#.as_bytes());
        assert_eq!(eval.next_token()?, Some(Token::StartTag(start_tag)));
        assert_eq!(eval.position(), 19);
        assert_eq!(eval.next_token()?, Some(Token::Eof));
        assert_eq!(eval.position(), 19);
        assert_eq!(eval.next_token()?, None);
        assert_eq!(eval.position(), 19);
        Ok(())
    }

    #[test]
    fn start_tag_with_single_quote_attribute_with_angle_bracket() -> Result<()> {
        let xml = r#"<hello name='ru>st'>"#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut eval = BufReadEvaluator::from_reader(bufreader);
        let start_tag = StartTag::from(r#"<hello name='ru>st'>"#.as_bytes());
        assert_eq!(eval.next_token()?, Some(Token::StartTag(start_tag)));
        assert_eq!(eval.position(), 20);
        assert_eq!(eval.next_token()?, Some(Token::Eof));
        assert_eq!(eval.position(), 20);
        assert_eq!(eval.next_token()?, None);
        assert_eq!(eval.position(), 20);
        Ok(())
    }

    #[test]
    fn end_tag() -> Result<()> {
        let xml = r"</goodbye>";
        let bufreader = TestBufRead::with_bytes(xml);
        let mut eval = BufReadEvaluator::from_reader(bufreader);
        let end_tag = EndTag::from(r#"</goodbye>"#.as_bytes());
        assert_eq!(eval.next_token()?, Some(Token::EndTag(end_tag)));
        assert_eq!(eval.position(), 10);
        assert_eq!(eval.next_token()?, Some(Token::Eof));
        assert_eq!(eval.position(), 10);
        assert_eq!(eval.next_token()?, None);
        assert_eq!(eval.position(), 10);
        Ok(())
    }

    #[test]
    fn empty_end_tag() -> Result<()> {
        let xml = r"</>";
        let bufreader = TestBufRead::with_bytes(xml);
        let mut eval = BufReadEvaluator::from_reader(bufreader);
        let end_tag = EndTag::from(r#"</>"#.as_bytes());
        assert_eq!(eval.next_token()?, Some(Token::EndTag(end_tag)));
        assert_eq!(eval.position(), 3);
        assert_eq!(eval.next_token()?, Some(Token::Eof));
        assert_eq!(eval.position(), 3);
        assert_eq!(eval.next_token()?, None);
        assert_eq!(eval.position(), 3);
        Ok(())
    }

    #[test]
    fn empty_element_tag() -> Result<()> {
        let xml = r"<standalone/>";
        let bufreader = TestBufRead::with_bytes(xml);
        let mut eval = BufReadEvaluator::from_reader(bufreader);
        let empty_element_tag = EmptyElementTag::from(r#"<standalone/>"#.as_bytes());
        assert_eq!(
            eval.next_token()?,
            Some(Token::EmptyElementTag(empty_element_tag))
        );
        assert_eq!(eval.position(), 13);
        assert_eq!(eval.next_token()?, Some(Token::Eof));
        assert_eq!(eval.position(), 13);
        assert_eq!(eval.next_token()?, None);
        assert_eq!(eval.position(), 13);
        Ok(())
    }

    #[test]
    fn processing_instruction() -> Result<()> {
        let xml = r#"<?xml-stylesheet type="text/css" href="example.css"?>"#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut eval = BufReadEvaluator::from_reader(bufreader);
        let processing_instruction = ProcessingInstruction::from(
            r#"<?xml-stylesheet type="text/css" href="example.css"?>"#.as_bytes(),
        );
        assert_eq!(
            eval.next_token()?,
            Some(Token::ProcessingInstruction(processing_instruction))
        );
        assert_eq!(eval.position(), 53);
        assert_eq!(eval.next_token()?, Some(Token::Eof));
        assert_eq!(eval.position(), 53);
        assert_eq!(eval.next_token()?, None);
        assert_eq!(eval.position(), 53);
        Ok(())
    }

    #[test]
    fn declaration() -> Result<()> {
        let xml = r#"<!DOCTYPE example>"#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut eval = BufReadEvaluator::from_reader(bufreader);
        let declaration = Declaration::from(r#"<!DOCTYPE example>"#.as_bytes());
        assert_eq!(eval.next_token()?, Some(Token::Declaration(declaration)));
        assert_eq!(eval.position(), 18);
        assert_eq!(eval.next_token()?, Some(Token::Eof));
        assert_eq!(eval.position(), 18);
        assert_eq!(eval.next_token()?, None);
        assert_eq!(eval.position(), 18);
        Ok(())
    }

    #[test]
    fn comment() -> Result<()> {
        let xml = r#"<!-- Example -->"#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut eval = BufReadEvaluator::from_reader(bufreader);
        let comment = Comment::from(r#"<!-- Example -->"#.as_bytes());
        assert_eq!(eval.next_token()?, Some(Token::Comment(comment)));
        assert_eq!(eval.position(), 16);
        assert_eq!(eval.next_token()?, Some(Token::Eof));
        assert_eq!(eval.position(), 16);
        assert_eq!(eval.next_token()?, None);
        assert_eq!(eval.position(), 16);
        Ok(())
    }

    #[test]
    fn cdata() -> Result<()> {
        let xml = r#"<![CDATA[ <Example> ]]>"#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut eval = BufReadEvaluator::from_reader(bufreader);
        let cdata = Cdata::from(r#"<![CDATA[ <Example> ]]>"#.as_bytes());
        assert_eq!(eval.next_token()?, Some(Token::Cdata(cdata)));
        assert_eq!(eval.position(), 23);
        assert_eq!(eval.next_token()?, Some(Token::Eof));
        assert_eq!(eval.position(), 23);
        assert_eq!(eval.next_token()?, None);
        assert_eq!(eval.position(), 23);
        Ok(())
    }

    #[test]
    fn bytes_not_evaluated() -> Result<()> {
        let xml = r#"<unfinished name="xml""#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut eval = BufReadEvaluator::from_reader(bufreader);
        let bytes_not_evaluated = BytesNotEvaluated::from(r#"<unfinished name="xml""#.as_bytes());
        assert_eq!(
            eval.next_token()?,
            Some(Token::EofWithBytesNotEvaluated(bytes_not_evaluated))
        );
        assert_eq!(eval.position(), 22);
        assert_eq!(eval.next_token()?, None);
        assert_eq!(eval.position(), 22);
        Ok(())
    }

    #[test]
    fn simple_xml_read() -> Result<()> {
        let xml = r#"<hello name="rust">Welcome!<goodbye/></hello><abcd></abcd>"#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut eval = BufReadEvaluator::from_reader(bufreader);
        let start_tag = StartTag::from(r#"<hello name="rust">"#.as_bytes());
        assert_eq!(eval.next_token()?, Some(Token::StartTag(start_tag)));
        assert_eq!(eval.position(), 19);

        let text_content = Characters::from(r#"Welcome!"#.as_bytes());
        assert_eq!(eval.next_token()?, Some(Token::Characters(text_content)));
        assert_eq!(eval.position(), 27);

        let empty_element_tag = EmptyElementTag::from(r#"<goodbye/>"#.as_bytes());
        assert_eq!(
            eval.next_token()?,
            Some(Token::EmptyElementTag(empty_element_tag))
        );
        assert_eq!(eval.position(), 37);

        let end_tag = EndTag::from(r#"</hello>"#.as_bytes());
        assert_eq!(eval.next_token()?, Some(Token::EndTag(end_tag)));
        assert_eq!(eval.position(), 45);

        let start_tag = StartTag::from(r#"<abcd>"#.as_bytes());
        assert_eq!(eval.next_token()?, Some(Token::StartTag(start_tag)));
        assert_eq!(eval.position(), 51);

        let end_tag = EndTag::from(r#"</abcd>"#.as_bytes());
        assert_eq!(eval.next_token()?, Some(Token::EndTag(end_tag)));
        assert_eq!(eval.position(), 58);

        assert_eq!(eval.next_token()?, Some(Token::Eof));
        assert_eq!(eval.position(), 58);

        assert_eq!(eval.next_token()?, None);
        assert_eq!(eval.position(), 58);

        Ok(())
    }

    #[test]
    fn simple_xml_read_with_space() -> Result<()> {
        let xml = r#"   <hello name="rust"> Welcome! <goodbye  />  </hello>  <abcd> </abcd> "#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut eval = BufReadEvaluator::from_reader(bufreader);

        let text_content = Characters::from(b"   ".as_ref());
        assert_eq!(eval.next_token()?, Some(Token::Characters(text_content)));
        assert_eq!(eval.position(), 3);

        let start_tag = StartTag::from(r#"<hello name="rust">"#.as_bytes());
        assert_eq!(eval.next_token()?, Some(Token::StartTag(start_tag)));
        assert_eq!(eval.position(), 22);

        let text_content = Characters::from(b" Welcome! ".as_ref());
        assert_eq!(eval.next_token()?, Some(Token::Characters(text_content)));
        assert_eq!(eval.position(), 32);

        let empty_element_tag = EmptyElementTag::from(r#"<goodbye  />"#.as_bytes());
        assert_eq!(
            eval.next_token()?,
            Some(Token::EmptyElementTag(empty_element_tag))
        );
        assert_eq!(eval.position(), 44);

        let text_content = Characters::from(b"  ".as_ref());
        assert_eq!(eval.next_token()?, Some(Token::Characters(text_content)));
        assert_eq!(eval.position(), 46);

        let end_tag = EndTag::from(r#"</hello>"#.as_bytes());
        assert_eq!(eval.next_token()?, Some(Token::EndTag(end_tag)));
        assert_eq!(eval.position(), 54);

        let text_content = Characters::from(b"  ".as_ref());
        assert_eq!(eval.next_token()?, Some(Token::Characters(text_content)));
        assert_eq!(eval.position(), 56);

        let start_tag = StartTag::from(r#"<abcd>"#.as_bytes());
        assert_eq!(eval.next_token()?, Some(Token::StartTag(start_tag)));
        assert_eq!(eval.position(), 62);

        let text_content = Characters::from(b" ".as_ref());
        assert_eq!(eval.next_token()?, Some(Token::Characters(text_content)));
        assert_eq!(eval.position(), 63);

        let end_tag = EndTag::from(r#"</abcd>"#.as_bytes());
        assert_eq!(eval.next_token()?, Some(Token::EndTag(end_tag)));
        assert_eq!(eval.position(), 70);

        let text_content = Characters::from(b" ".as_ref());
        assert_eq!(eval.next_token()?, Some(Token::Characters(text_content)));
        assert_eq!(eval.position(), 71);

        assert_eq!(eval.next_token()?, Some(Token::Eof));
        assert_eq!(eval.position(), 71);

        assert_eq!(eval.next_token()?, None);
        assert_eq!(eval.position(), 71);

        Ok(())
    }

    #[test]
    fn iter_empty() -> Result<()> {
        use crate::token::owned::Token;

        let xml = r"";
        let mut iter = BufReadEvaluator::from_reader(xml.as_bytes()).into_iter();
        assert_eq!(iter.next(), Some(Token::Eof));
        assert_eq!(iter.position(), 0);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.position(), 0);
        Ok(())
    }

    #[test]
    fn iter_characters_content() -> Result<()> {
        use crate::token::owned::{Characters, Token};

        let xml = r"hello world";
        let bufreader = TestBufRead::with_bytes(xml);
        let mut iter = BufReadEvaluator::from_reader(bufreader).into_iter();
        let text_content = Characters::from(b"hello world".as_ref());
        assert_eq!(iter.next(), Some(Token::Characters(text_content)));
        assert_eq!(iter.position(), 11);
        assert_eq!(iter.next(), Some(Token::Eof));
        assert_eq!(iter.position(), 11);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.position(), 11);
        Ok(())
    }

    #[test]
    fn iter_start_tag() -> Result<()> {
        use crate::token::owned::{StartTag, Token};

        let xml = r"<hello>";
        let bufreader = TestBufRead::with_bytes(xml);
        let mut iter = BufReadEvaluator::from_reader(bufreader).into_iter();
        let start_tag = StartTag::from(b"<hello>".as_ref());
        assert_eq!(iter.next(), Some(Token::StartTag(start_tag)));
        assert_eq!(iter.position(), 7);
        assert_eq!(iter.next(), Some(Token::Eof));
        assert_eq!(iter.position(), 7);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.position(), 7);
        Ok(())
    }

    #[test]
    fn iter_start_tag_with_double_quote_attribute() -> Result<()> {
        use crate::token::owned::{StartTag, Token};

        let xml = r#"<hello name="rust">"#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut iter = BufReadEvaluator::from_reader(bufreader).into_iter();
        let start_tag = StartTag::from(r#"<hello name="rust">"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::StartTag(start_tag)));
        assert_eq!(iter.position(), 19);
        assert_eq!(iter.next(), Some(Token::Eof));
        assert_eq!(iter.position(), 19);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.position(), 19);
        Ok(())
    }

    #[test]
    fn iter_start_tag_with_double_quote_attribute_with_angle_bracket() -> Result<()> {
        use crate::token::owned::{StartTag, Token};

        let xml = r#"<hello name="ru>st">"#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut iter = BufReadEvaluator::from_reader(bufreader).into_iter();
        let start_tag = StartTag::from(r#"<hello name="ru>st">"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::StartTag(start_tag)));
        assert_eq!(iter.position(), 20);
        assert_eq!(iter.next(), Some(Token::Eof));
        assert_eq!(iter.position(), 20);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.position(), 20);
        Ok(())
    }

    #[test]
    fn iter_start_tag_with_single_quote_attribute() -> Result<()> {
        use crate::token::owned::{StartTag, Token};

        let xml = r#"<hello name='rust'>"#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut iter = BufReadEvaluator::from_reader(bufreader).into_iter();
        let start_tag = StartTag::from(r#"<hello name='rust'>"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::StartTag(start_tag)));
        assert_eq!(iter.position(), 19);
        assert_eq!(iter.next(), Some(Token::Eof));
        assert_eq!(iter.position(), 19);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.position(), 19);
        Ok(())
    }

    #[test]
    fn iter_start_tag_with_single_quote_attribute_with_angle_bracket() -> Result<()> {
        use crate::token::owned::{StartTag, Token};

        let xml = r#"<hello name='ru>st'>"#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut iter = BufReadEvaluator::from_reader(bufreader).into_iter();
        let start_tag = StartTag::from(r#"<hello name='ru>st'>"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::StartTag(start_tag)));
        assert_eq!(iter.position(), 20);
        assert_eq!(iter.next(), Some(Token::Eof));
        assert_eq!(iter.position(), 20);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.position(), 20);
        Ok(())
    }

    #[test]
    fn iter_end_tag() -> Result<()> {
        use crate::token::owned::{EndTag, Token};

        let xml = r"</goodbye>";
        let bufreader = TestBufRead::with_bytes(xml);
        let mut iter = BufReadEvaluator::from_reader(bufreader).into_iter();
        let end_tag = EndTag::from(r#"</goodbye>"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::EndTag(end_tag)));
        assert_eq!(iter.position(), 10);
        assert_eq!(iter.next(), Some(Token::Eof));
        assert_eq!(iter.position(), 10);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.position(), 10);
        Ok(())
    }

    #[test]
    fn iter_empty_end_tag() -> Result<()> {
        use crate::token::owned::{EndTag, Token};

        let xml = r"</>";
        let bufreader = TestBufRead::with_bytes(xml);
        let mut iter = BufReadEvaluator::from_reader(bufreader).into_iter();
        let end_tag = EndTag::from(r#"</>"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::EndTag(end_tag)));
        assert_eq!(iter.position(), 3);
        assert_eq!(iter.next(), Some(Token::Eof));
        assert_eq!(iter.position(), 3);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.position(), 3);
        Ok(())
    }

    #[test]
    fn iter_empty_element_tag() -> Result<()> {
        use crate::token::owned::{EmptyElementTag, Token};

        let xml = r"<standalone/>";
        let bufreader = TestBufRead::with_bytes(xml);
        let mut iter = BufReadEvaluator::from_reader(bufreader).into_iter();
        let empty_element_tag = EmptyElementTag::from(r#"<standalone/>"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::EmptyElementTag(empty_element_tag)));
        assert_eq!(iter.position(), 13);
        assert_eq!(iter.next(), Some(Token::Eof));
        assert_eq!(iter.position(), 13);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.position(), 13);
        Ok(())
    }

    #[test]
    fn iter_processing_instruction() -> Result<()> {
        use crate::token::owned::{ProcessingInstruction, Token};

        let xml = r#"<?xml-stylesheet type="text/css" href="example.css"?>"#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut iter = BufReadEvaluator::from_reader(bufreader).into_iter();
        let processing_instruction = ProcessingInstruction::from(
            r#"<?xml-stylesheet type="text/css" href="example.css"?>"#.as_bytes(),
        );
        assert_eq!(
            iter.next(),
            Some(Token::ProcessingInstruction(processing_instruction))
        );
        assert_eq!(iter.position(), 53);
        assert_eq!(iter.next(), Some(Token::Eof));
        assert_eq!(iter.position(), 53);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.position(), 53);
        Ok(())
    }

    #[test]
    fn iter_declaration() -> Result<()> {
        use crate::token::owned::{Declaration, Token};

        let xml = r#"<!DOCTYPE example>"#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut iter = BufReadEvaluator::from_reader(bufreader).into_iter();
        let declaration = Declaration::from(r#"<!DOCTYPE example>"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::Declaration(declaration)));
        assert_eq!(iter.position(), 18);
        assert_eq!(iter.next(), Some(Token::Eof));
        assert_eq!(iter.position(), 18);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.position(), 18);
        Ok(())
    }

    #[test]
    fn iter_comment() -> Result<()> {
        use crate::token::owned::{Comment, Token};

        let xml = r#"<!-- Example -->"#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut iter = BufReadEvaluator::from_reader(bufreader).into_iter();
        let comment = Comment::from(r#"<!-- Example -->"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::Comment(comment)));
        assert_eq!(iter.position(), 16);
        assert_eq!(iter.next(), Some(Token::Eof));
        assert_eq!(iter.position(), 16);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.position(), 16);
        Ok(())
    }

    #[test]
    fn iter_cdata() -> Result<()> {
        use crate::token::owned::{Cdata, Token};

        let xml = r#"<![CDATA[ <Example> ]]>"#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut iter = BufReadEvaluator::from_reader(bufreader).into_iter();
        let cdata = Cdata::from(r#"<![CDATA[ <Example> ]]>"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::Cdata(cdata)));
        assert_eq!(iter.position(), 23);
        assert_eq!(iter.next(), Some(Token::Eof));
        assert_eq!(iter.position(), 23);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.position(), 23);
        Ok(())
    }

    #[test]
    fn iter_bytes_not_evaluated() -> Result<()> {
        use crate::token::owned::{BytesNotEvaluated, Token};

        let xml = r#"<unfinished name="xml""#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut iter = BufReadEvaluator::from_reader(bufreader).into_iter();
        let bytes_not_evaluated = BytesNotEvaluated::from(r#"<unfinished name="xml""#.as_bytes());
        assert_eq!(
            iter.next(),
            Some(Token::EofWithBytesNotEvaluated(bytes_not_evaluated))
        );
        assert_eq!(iter.position(), 22);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.position(), 22);
        Ok(())
    }

    #[test]
    fn iter_simple_xml_read() -> Result<()> {
        use crate::token::owned::{Characters, EmptyElementTag, EndTag, StartTag, Token};

        let xml = r#"<hello name="rust">Welcome!<goodbye/></hello><abcd></abcd>"#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut iter = BufReadEvaluator::from_reader(bufreader).into_iter();
        let start_tag = StartTag::from(r#"<hello name="rust">"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::StartTag(start_tag)));
        assert_eq!(iter.position(), 19);

        let text_content = Characters::from(r#"Welcome!"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::Characters(text_content)));
        assert_eq!(iter.position(), 27);

        let empty_element_tag = EmptyElementTag::from(r#"<goodbye/>"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::EmptyElementTag(empty_element_tag)));
        assert_eq!(iter.position(), 37);

        let end_tag = EndTag::from(r#"</hello>"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::EndTag(end_tag)));
        assert_eq!(iter.position(), 45);

        let start_tag = StartTag::from(r#"<abcd>"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::StartTag(start_tag)));
        assert_eq!(iter.position(), 51);

        let end_tag = EndTag::from(r#"</abcd>"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::EndTag(end_tag)));
        assert_eq!(iter.position(), 58);

        assert_eq!(iter.next(), Some(Token::Eof));
        assert_eq!(iter.position(), 58);

        assert_eq!(iter.next(), None);
        assert_eq!(iter.position(), 58);

        Ok(())
    }

    #[test]
    fn iter_simple_xml_read_with_space() -> Result<()> {
        use crate::token::owned::{Characters, EmptyElementTag, EndTag, StartTag, Token};

        let xml = r#"   <hello name="rust"> Welcome! <goodbye  />  </hello>  <abcd> </abcd> "#;
        let bufreader = TestBufRead::with_bytes(xml);
        let mut iter = BufReadEvaluator::from_reader(bufreader).into_iter();

        let text_content = Characters::from(b"   ".as_ref());
        assert_eq!(iter.next(), Some(Token::Characters(text_content)));
        assert_eq!(iter.position(), 3);

        let start_tag = StartTag::from(r#"<hello name="rust">"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::StartTag(start_tag)));
        assert_eq!(iter.position(), 22);

        let text_content = Characters::from(b" Welcome! ".as_ref());
        assert_eq!(iter.next(), Some(Token::Characters(text_content)));
        assert_eq!(iter.position(), 32);

        let empty_element_tag = EmptyElementTag::from(r#"<goodbye  />"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::EmptyElementTag(empty_element_tag)));
        assert_eq!(iter.position(), 44);

        let text_content = Characters::from(b"  ".as_ref());
        assert_eq!(iter.next(), Some(Token::Characters(text_content)));
        assert_eq!(iter.position(), 46);

        let end_tag = EndTag::from(r#"</hello>"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::EndTag(end_tag)));
        assert_eq!(iter.position(), 54);

        let text_content = Characters::from(b"  ".as_ref());
        assert_eq!(iter.next(), Some(Token::Characters(text_content)));
        assert_eq!(iter.position(), 56);

        let start_tag = StartTag::from(r#"<abcd>"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::StartTag(start_tag)));
        assert_eq!(iter.position(), 62);

        let text_content = Characters::from(b" ".as_ref());
        assert_eq!(iter.next(), Some(Token::Characters(text_content)));
        assert_eq!(iter.position(), 63);

        let end_tag = EndTag::from(r#"</abcd>"#.as_bytes());
        assert_eq!(iter.next(), Some(Token::EndTag(end_tag)));
        assert_eq!(iter.position(), 70);

        let text_content = Characters::from(b" ".as_ref());
        assert_eq!(iter.next(), Some(Token::Characters(text_content)));
        assert_eq!(iter.position(), 71);

        assert_eq!(iter.next(), Some(Token::Eof));
        assert_eq!(iter.position(), 71);

        assert_eq!(iter.next(), None);
        assert_eq!(iter.position(), 71);

        Ok(())
    }
}
