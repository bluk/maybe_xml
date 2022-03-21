// Copyright 2022 Bryant Luk
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Provides an iterator and cursor API for a stream of manually managed byte slices.

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use std::vec::Vec;

#[cfg(any(feature = "std", feature = "alloc"))]
use crate::{
    scanner::{Scanner, State as ScannerState},
    token::borrowed,
};
#[cfg(any(feature = "std", feature = "alloc"))]
use core::{convert::TryFrom, fmt};

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
#[cfg(any(feature = "std", feature = "alloc"))]
enum State {
    Reset,
    Evaluating,
    EvaluatedStartTag,
    EvaluatedEmptyElementTag,
    EvaluatedEndTag,
    EvaluatedProcessingInstruction,
    EvaluatedDeclaration,
    EvaluatedTextContent,
    EvaluatedComment,
    EvaluatedCdata,
    Eof,
    Done,
}

/// Errors when trying to process the stream of bytes into a token.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg(any(feature = "std", feature = "alloc"))]
#[non_exhaustive]
pub enum RecvError {
    /// If the current token is not determined to be finished, more bytes must be
    /// given to the evaluator.
    NeedToRecvMoreBytes,
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl fmt::Display for RecvError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RecvError::NeedToRecvMoreBytes => write!(f, "need to receive more bytes"),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for RecvError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

/// Receives a stream of manually managed byte slices and returns a token when a token is determined to be complete.
///
/// In most cases, the `maybe_xml::eval::bufread::BufReadEvaluator` is easier to use, but for `no_std` environments
/// which still have an allocator, then `RecvEvaluator` may be useful.
///
/// # Usage
///
/// Suppose that the contents of a HTTP response is being received as a stream of byte slices.
/// A bytes slice is passed into the evaluator via the `recv()` method. The returned value
/// is the number of bytes which are considered consumed by the evaluator. The calling code
/// should consume (e.g. discard) the same number of bytes in the byte slice.
///
/// Then, a `next_token()` call should be made to determine if the evaluator
/// has received enough bytes for a complete token. If the evaluator has not received enough bytes,
/// an `Err(Error::NeedToRecvMoreBytes)` is returned and the code should wait until more bytes from
/// the HTTP response are received before calling `recv()` with the newly received bytes.
///
/// If the evaluator has determined there are enough bytes received for a
/// complete token, `Ok(Some(token))` is returned where `token` is the actual
/// token type. Once the calling code is done reading the token, repeat the
/// process starting with giving the evaluator more bytes via the `recv()`
/// method.
///
/// # Example
///
/// Normally, bytes are read from an input source and then passed to the `RecvEvaluator` in a loop.
/// In the following example, the loop is unrolled to show possible return values
/// and what should happen more clearly.
///
/// ```
/// use maybe_xml::{
///     token::borrowed::{StartTag, Characters, EndTag, Token},
///     eval::recv::{RecvEvaluator, RecvError}
/// };
///
/// let mut eval = RecvEvaluator::new();
///
/// // Bytes have been read from the underlying XML source
///
/// let input = r#"<id"#.as_bytes();
/// let read = eval.recv(input);
/// let result = eval.next_token();
/// // The token is not complete, so `RecvError::NeedToRecvMoreBytes` is returned.
/// assert_eq!(result, Err(RecvError::NeedToRecvMoreBytes));
/// // All of the bytes have been read, so consume/drop the `input` bytes.
/// assert_eq!(read, input.len());
///
/// // More bytes have been read from the underlying XML source
///
/// let more_input = r#">12345<"#.as_bytes();
/// let read = eval.recv(more_input);
/// let result = eval.next_token();
/// assert_eq!(result, Ok(Some(Token::StartTag(StartTag::from("<id>")))));
/// assert_eq!(read, 1);
/// // Note that `read` is not equal to `more_input.len()`
/// assert_ne!(read, more_input.len());
///
/// // Only some of the bytes in `more_input` were read by the evaluator,
/// // so, pass the remaining unread bytes.
/// let more_input = &more_input[read..];
/// assert_eq!(more_input, b"12345<");
/// let read = eval.recv(more_input);
/// let result = eval.next_token();
/// assert_eq!(
///     result,
///     Ok(Some(Token::Characters(Characters::from("12345"))))
/// );
/// assert_eq!(read, 5);
/// // Note that `read` is not equal to `more_input.len()`
/// assert_ne!(read, more_input.len());
///
/// // The `more_input` buffer is still not completely read by the evaluator,
/// // so pass the still remaining bytes again.
/// let more_input = &more_input[read..];
/// assert_eq!(more_input, b"<");
/// let read = eval.recv(more_input);
/// let result = eval.next_token();
/// // The token is not complete, so `RecvError::NeedToRecvMoreBytes` is returned.
/// assert_eq!(result, Err(RecvError::NeedToRecvMoreBytes));
/// assert_eq!(read, 1);
/// // All of the bytes have been read, so consume/drop the `more_input` bytes.
/// assert_eq!(read, more_input.len());
///
/// // Even more bytes have been read from the underlying XML source
///
/// let even_more_input = r#"/id>"#.as_bytes();
/// let read = eval.recv(even_more_input);
/// let result = eval.next_token();
/// assert_eq!(result, Ok(Some(Token::EndTag(EndTag::from("</id>")))));
/// assert_eq!(read, 4);
/// // All of the bytes have been read, so consume/drop the `even_more_input` bytes.
/// assert_eq!(read, even_more_input.len());
///
/// // An empty byte slice (signaling the end of the file) has been
/// // read from the underlying XML source
///
/// let last_input = r#""#.as_bytes();
/// let read = eval.recv(last_input);
/// let result = eval.next_token();
/// assert_eq!(result, Ok(Some(Token::Eof)));
/// assert_eq!(read, 0);
///
/// // All future calls to `recv` or `next_token` will not process any more data.
/// let unused_input = r#"<unused>"#.as_bytes();
/// let read = eval.recv(unused_input);
/// assert_eq!(read, 0);
/// let result = eval.next_token();
/// assert_eq!(result, Ok(None));
/// ```
#[derive(Clone, Debug)]
#[cfg(any(feature = "std", feature = "alloc"))]
pub struct RecvEvaluator {
    buf: Vec<u8>,
    scanner: Scanner,
    position: u64,
    state: State,
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl RecvEvaluator {
    /// Instantiates a new evaluator.
    pub fn new() -> Self {
        Self {
            buf: Vec::new(),
            scanner: Scanner::new(),
            position: 0,
            state: State::Evaluating,
        }
    }

    /// Returns the current index position if the received byte slices were a continuous byte sequence.
    ///
    /// The position is mostly useful for debugging.
    pub fn position(&self) -> u64 {
        self.position
    }

    /// Passes bytes to the evaluator which are stored into an internal buffer.
    ///
    /// The return value is the number of bytes "consumed" by the evaluator. If the return
    /// value is stored in a variable called `read_count`, then `&bytes[..read_count]` should
    /// be consumed by the callling code. In other words, in another future call to `recv()`,
    /// only `&bytes[read_count..]` should be passed in.
    ///
    /// The `recv()` method only consumes enough bytes to complete the current token.
    ///
    /// # Important
    ///
    /// Passing an empty byte slice indicates the end of file state has been reached. Once
    /// the end of file state is reached, the evaluator will not process more bytes in the
    /// `recv()` method.
    ///
    /// The `next_token()` method should always be called after a call to `recv()`. Suppose
    /// `recv()` is called and a complete token is formed. If `recv()` is called **without**
    /// a `next_token()` call, the completed token is discarded and the evaluator
    /// will start buffering for a new token.
    pub fn recv(&mut self, bytes: &[u8]) -> usize {
        match self.state {
            State::Evaluating => {}
            State::Reset
            | State::EvaluatedStartTag
            | State::EvaluatedEmptyElementTag
            | State::EvaluatedEndTag
            | State::EvaluatedProcessingInstruction
            | State::EvaluatedDeclaration
            | State::EvaluatedComment
            | State::EvaluatedCdata
            | State::EvaluatedTextContent => self.buf.clear(),
            State::Eof => {
                self.state = State::Done;
                return 0;
            }
            State::Done => {
                return 0;
            }
        }

        let state = self.scanner.scan(bytes);
        match state {
            None => {
                self.state = State::Eof;
                0
            }
            Some(ScannerState::ScanningMarkup) => {
                self.buf.extend_from_slice(bytes);
                let bytes_len = bytes.len();
                self.position += u64::try_from(bytes_len)
                    .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                self.state = State::Evaluating;
                bytes_len
            }
            Some(ScannerState::ScanningStartOrEmptyElementTag) => {
                self.buf.extend_from_slice(bytes);
                let bytes_len = bytes.len();
                self.position += u64::try_from(bytes_len)
                    .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                self.state = State::Evaluating;
                bytes_len
            }
            Some(ScannerState::ScanningCharacters) => {
                self.buf.extend_from_slice(bytes);
                let bytes_len = bytes.len();
                self.position += u64::try_from(bytes_len)
                    .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                self.state = State::Evaluating;
                bytes_len
            }
            Some(ScannerState::ScanningEndTag) => {
                self.buf.extend_from_slice(bytes);
                let bytes_len = bytes.len();
                self.position += u64::try_from(bytes_len)
                    .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                self.state = State::Evaluating;
                bytes_len
            }
            Some(ScannerState::ScanningProcessingInstruction) => {
                self.buf.extend_from_slice(bytes);
                let bytes_len = bytes.len();
                self.position += u64::try_from(bytes_len)
                    .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                self.state = State::Evaluating;
                bytes_len
            }
            Some(ScannerState::ScanningDeclarationCommentOrCdata) => {
                self.buf.extend_from_slice(bytes);
                let bytes_len = bytes.len();
                self.position += u64::try_from(bytes_len)
                    .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                self.state = State::Evaluating;
                bytes_len
            }
            Some(ScannerState::ScanningDeclaration) => {
                self.buf.extend_from_slice(bytes);
                let bytes_len = bytes.len();
                self.position += u64::try_from(bytes_len)
                    .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                self.state = State::Evaluating;
                bytes_len
            }
            Some(ScannerState::ScanningComment) => {
                self.buf.extend_from_slice(bytes);
                let bytes_len = bytes.len();
                self.position += u64::try_from(bytes_len)
                    .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                self.state = State::Evaluating;
                bytes_len
            }
            Some(ScannerState::ScanningCdata) => {
                self.buf.extend_from_slice(bytes);
                let bytes_len = bytes.len();
                self.position += u64::try_from(bytes_len)
                    .unwrap_or_else(|_| panic!("{} to be <= u64::MAX", bytes_len));
                self.state = State::Evaluating;
                bytes_len
            }
            Some(ScannerState::ScannedStartTag(read)) => {
                self.buf.extend_from_slice(&bytes[..read]);
                self.position +=
                    u64::try_from(read).unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                self.state = State::EvaluatedStartTag;
                read
            }
            Some(ScannerState::ScannedEmptyElementTag(read)) => {
                self.buf.extend_from_slice(&bytes[..read]);
                self.position +=
                    u64::try_from(read).unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                self.state = State::EvaluatedEmptyElementTag;
                read
            }
            Some(ScannerState::ScannedEndTag(read)) => {
                self.buf.extend_from_slice(&bytes[..read]);
                self.position +=
                    u64::try_from(read).unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                self.state = State::EvaluatedEndTag;
                read
            }
            Some(ScannerState::ScannedCharacters(read)) => {
                self.buf.extend_from_slice(&bytes[..read]);
                self.position +=
                    u64::try_from(read).unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                self.state = State::EvaluatedTextContent;
                read
            }
            Some(ScannerState::ScannedProcessingInstruction(read)) => {
                self.buf.extend_from_slice(&bytes[..read]);
                self.position +=
                    u64::try_from(read).unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                self.state = State::EvaluatedProcessingInstruction;
                read
            }
            Some(ScannerState::ScannedDeclaration(read)) => {
                self.buf.extend_from_slice(&bytes[..read]);
                self.position +=
                    u64::try_from(read).unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                self.state = State::EvaluatedDeclaration;
                read
            }
            Some(ScannerState::ScannedComment(read)) => {
                self.buf.extend_from_slice(&bytes[..read]);
                self.position +=
                    u64::try_from(read).unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                self.state = State::EvaluatedComment;
                read
            }
            Some(ScannerState::ScannedCdata(read)) => {
                self.buf.extend_from_slice(&bytes[..read]);
                self.position +=
                    u64::try_from(read).unwrap_or_else(|_| panic!("{} to be <= u64::MAX", read));
                self.state = State::EvaluatedCdata;
                read
            }
        }
    }

    /// Returns the completed token if available.
    ///
    /// # Errors
    ///
    /// If a complete token has not been determined yet, then a `Err(RecvError:NeedToRecvMoreBytes)`
    /// will be returned. It is not a fatal error, but more bytes must be passed into the evaluator
    /// via the `recv()` method.
    ///
    /// # Important
    ///
    /// The `next_token()` method should always be called after every `recv()` call.
    ///
    /// A completed token is only returned once from a `next_token()` call, even if no
    /// `recv()` calls are made after the `next_token()` call.
    ///
    /// If the end of file state is reached, `next_token()` will return `Ok(Some(Token::Eof))`
    /// or `Ok(Some(Token::EofWithBytesNotEvaluated))` once. Then any further calls to the method will always
    /// return `Ok(None)`.
    pub fn next_token(&mut self) -> Result<Option<borrowed::Token>, RecvError> {
        use borrowed::{
            BytesNotEvaluated, Cdata, Characters, Comment, Declaration, EmptyElementTag, EndTag,
            ProcessingInstruction, StartTag, Token,
        };

        let token = match self.state {
            State::Reset | State::Evaluating => return Err(RecvError::NeedToRecvMoreBytes),
            State::EvaluatedStartTag => {
                self.state = State::Reset;
                Some(Token::StartTag(StartTag::from(&self.buf)))
            }
            State::EvaluatedEndTag => {
                self.state = State::Reset;
                Some(Token::EndTag(EndTag::from(&self.buf)))
            }
            State::EvaluatedEmptyElementTag => {
                self.state = State::Reset;
                Some(Token::EmptyElementTag(EmptyElementTag::from(&self.buf)))
            }
            State::EvaluatedTextContent => {
                self.state = State::Reset;
                Some(Token::Characters(Characters::from(&self.buf)))
            }
            State::EvaluatedProcessingInstruction => {
                self.state = State::Reset;
                Some(Token::ProcessingInstruction(ProcessingInstruction::from(
                    &self.buf,
                )))
            }
            State::EvaluatedDeclaration => {
                self.state = State::Reset;
                Some(Token::Declaration(Declaration::from(&self.buf)))
            }
            State::EvaluatedComment => {
                self.state = State::Reset;
                Some(Token::Comment(Comment::from(&self.buf)))
            }
            State::EvaluatedCdata => {
                self.state = State::Reset;
                Some(Token::Cdata(Cdata::from(&self.buf)))
            }
            State::Eof => {
                self.state = State::Done;
                if self.buf.is_empty() {
                    Some(Token::Eof)
                } else {
                    Some(Token::EofWithBytesNotEvaluated(BytesNotEvaluated::from(
                        &self.buf,
                    )))
                }
            }
            State::Done => None,
        };

        Ok(token)
    }
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl Default for RecvEvaluator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(all(test, any(feature = "std", feature = "alloc")))]
mod tests {
    use super::*;
    use crate::token::borrowed::{
        BytesNotEvaluated, Cdata, Characters, Comment, Declaration, EmptyElementTag, EndTag,
        ProcessingInstruction, StartTag, Token,
    };
    use rand::{thread_rng, Rng};

    type Result<T> = core::result::Result<T, RecvError>;

    struct TestRecv {
        bytes: Vec<u8>,
        index: usize,
    }

    impl TestRecv {
        fn with_bytes<B: AsRef<[u8]>>(bytes: B) -> Self {
            Self {
                bytes: bytes.as_ref().to_vec(),
                index: 0,
            }
        }

        fn assert_next_token(
            &mut self,
            eval: &mut RecvEvaluator,
            expected_token: Option<Token>,
        ) -> bool {
            let mut rng = thread_rng();
            let mut fill_size = rng.gen_range(1..self.bytes.len() - self.index + 2);
            loop {
                let end = usize::min(self.index + fill_size, self.bytes.len());
                let bytes = &self.bytes[self.index..end];
                let read = eval.recv(bytes);
                self.index += read;

                let next_token = eval.next_token();
                if let Ok(token) = next_token {
                    assert_eq!(token, expected_token);
                    return true;
                } else {
                    assert_eq!(next_token, Err(RecvError::NeedToRecvMoreBytes));
                    if self.index == self.bytes.len() {
                        if expected_token.is_some() {
                            let read = eval.recv(&[]);
                            assert_eq!(read, 0);
                            let next_token = eval.next_token();
                            assert_eq!(next_token, Ok(expected_token));
                            return true;
                        }
                        return expected_token.is_none();
                    }
                }

                if self.index + fill_size < self.bytes.len() {
                    fill_size += rng.gen_range(1..self.bytes.len() - self.index - fill_size + 2);
                }
            }
        }
    }

    #[test]
    fn empty() -> Result<()> {
        let xml = r"";
        let mut eval = RecvEvaluator::new();
        assert_eq!(eval.recv(xml.as_bytes()), 0);
        assert_eq!(eval.next_token()?, Some(Token::Eof));
        assert_eq!(eval.position(), 0);
        assert_eq!(eval.recv(xml.as_bytes()), 0);
        assert_eq!(eval.next_token()?, None);
        assert_eq!(eval.position(), 0);
        Ok(())
    }

    #[test]
    fn characters_content() -> Result<()> {
        let xml = r"hello world";
        let mut tester = TestRecv::with_bytes(xml);
        let mut eval = RecvEvaluator::new();
        let text_content = Characters::from(b"hello world".as_ref());
        assert!(tester.assert_next_token(&mut eval, Some(Token::Characters(text_content))));
        assert_eq!(eval.position(), 11);
        assert!(tester.assert_next_token(&mut eval, Some(Token::Eof)));
        assert_eq!(eval.position(), 11);
        assert!(tester.assert_next_token(&mut eval, None));
        assert_eq!(eval.position(), 11);
        Ok(())
    }

    #[test]
    fn start_tag() -> Result<()> {
        let xml = r"<hello>";
        let mut tester = TestRecv::with_bytes(xml);
        let mut eval = RecvEvaluator::new();
        let start_tag = StartTag::from(b"<hello>".as_ref());
        assert!(tester.assert_next_token(&mut eval, Some(Token::StartTag(start_tag))));
        assert_eq!(eval.position(), 7);
        assert!(tester.assert_next_token(&mut eval, Some(Token::Eof)));
        assert_eq!(eval.position(), 7);
        assert!(tester.assert_next_token(&mut eval, None));
        assert_eq!(eval.position(), 7);
        Ok(())
    }

    #[test]
    fn start_tag_with_double_quote_attribute() -> Result<()> {
        let xml = r#"<hello name="rust">"#;
        let mut tester = TestRecv::with_bytes(xml);
        let mut eval = RecvEvaluator::new();
        let start_tag = StartTag::from(r#"<hello name="rust">"#.as_bytes());
        assert!(tester.assert_next_token(&mut eval, Some(Token::StartTag(start_tag))));
        assert_eq!(eval.position(), 19);
        assert!(tester.assert_next_token(&mut eval, Some(Token::Eof)));
        assert_eq!(eval.position(), 19);
        assert!(tester.assert_next_token(&mut eval, None));
        assert_eq!(eval.position(), 19);
        Ok(())
    }

    #[test]
    fn start_tag_with_double_quote_attribute_with_angle_bracket() -> Result<()> {
        let xml = r#"<hello name="ru>st">"#;
        let mut tester = TestRecv::with_bytes(xml);
        let mut eval = RecvEvaluator::new();
        let start_tag = StartTag::from(r#"<hello name="ru>st">"#.as_bytes());
        assert!(tester.assert_next_token(&mut eval, Some(Token::StartTag(start_tag))),);
        assert_eq!(eval.position(), 20);
        assert!(tester.assert_next_token(&mut eval, Some(Token::Eof)));
        assert_eq!(eval.position(), 20);
        assert!(tester.assert_next_token(&mut eval, None));
        assert_eq!(eval.position(), 20);
        Ok(())
    }

    #[test]
    fn start_tag_with_single_quote_attribute() -> Result<()> {
        let xml = r#"<hello name='rust'>"#;
        let mut tester = TestRecv::with_bytes(xml);
        let mut eval = RecvEvaluator::new();
        let start_tag = StartTag::from(r#"<hello name='rust'>"#.as_bytes());
        assert!(tester.assert_next_token(&mut eval, Some(Token::StartTag(start_tag))));
        assert_eq!(eval.position(), 19);
        assert!(tester.assert_next_token(&mut eval, Some(Token::Eof)));
        assert_eq!(eval.position(), 19);
        assert!(tester.assert_next_token(&mut eval, None));
        assert_eq!(eval.position(), 19);
        Ok(())
    }

    #[test]
    fn start_tag_with_single_quote_attribute_with_angle_bracket() -> Result<()> {
        let xml = r#"<hello name='ru>st'>"#;
        let mut tester = TestRecv::with_bytes(xml);
        let mut eval = RecvEvaluator::new();
        let start_tag = StartTag::from(r#"<hello name='ru>st'>"#.as_bytes());
        assert!(tester.assert_next_token(&mut eval, Some(Token::StartTag(start_tag))));
        assert_eq!(eval.position(), 20);
        assert!(tester.assert_next_token(&mut eval, Some(Token::Eof)));
        assert_eq!(eval.position(), 20);
        assert!(tester.assert_next_token(&mut eval, None));
        assert_eq!(eval.position(), 20);
        Ok(())
    }

    #[test]
    fn end_tag() -> Result<()> {
        let xml = r"</goodbye>";
        let mut tester = TestRecv::with_bytes(xml);
        let mut eval = RecvEvaluator::new();
        let end_tag = EndTag::from(r#"</goodbye>"#.as_bytes());
        assert!(tester.assert_next_token(&mut eval, Some(Token::EndTag(end_tag))));
        assert_eq!(eval.position(), 10);
        assert!(tester.assert_next_token(&mut eval, Some(Token::Eof)));
        assert_eq!(eval.position(), 10);
        assert!(tester.assert_next_token(&mut eval, None));
        assert_eq!(eval.position(), 10);
        Ok(())
    }

    #[test]
    fn empty_end_tag() -> Result<()> {
        let xml = r"</>";
        let mut tester = TestRecv::with_bytes(xml);
        let mut eval = RecvEvaluator::new();
        let end_tag = EndTag::from(r#"</>"#.as_bytes());
        assert!(tester.assert_next_token(&mut eval, Some(Token::EndTag(end_tag))));
        assert_eq!(eval.position(), 3);
        assert!(tester.assert_next_token(&mut eval, Some(Token::Eof)));
        assert_eq!(eval.position(), 3);
        assert!(tester.assert_next_token(&mut eval, None));
        assert_eq!(eval.position(), 3);
        Ok(())
    }

    #[test]
    fn empty_element_tag() -> Result<()> {
        let xml = r"<standalone/>";
        let mut tester = TestRecv::with_bytes(xml);
        let mut eval = RecvEvaluator::new();
        let empty_element_tag = EmptyElementTag::from(r#"<standalone/>"#.as_bytes());
        assert!(
            tester.assert_next_token(&mut eval, Some(Token::EmptyElementTag(empty_element_tag)))
        );
        assert_eq!(eval.position(), 13);
        assert!(tester.assert_next_token(&mut eval, Some(Token::Eof)));
        assert_eq!(eval.position(), 13);
        assert!(tester.assert_next_token(&mut eval, None));
        assert_eq!(eval.position(), 13);
        Ok(())
    }

    #[test]
    fn processing_instruction() -> Result<()> {
        let xml = r#"<?xml-stylesheet type="text/css" href="example.css"?>"#;
        let mut tester = TestRecv::with_bytes(xml);
        let mut eval = RecvEvaluator::new();
        let processing_instruction = ProcessingInstruction::from(
            r#"<?xml-stylesheet type="text/css" href="example.css"?>"#.as_bytes(),
        );
        assert!(tester.assert_next_token(
            &mut eval,
            Some(Token::ProcessingInstruction(processing_instruction))
        ));
        assert_eq!(eval.position(), 53);
        assert!(tester.assert_next_token(&mut eval, Some(Token::Eof)));
        assert_eq!(eval.position(), 53);
        assert!(tester.assert_next_token(&mut eval, None));
        assert_eq!(eval.position(), 53);
        Ok(())
    }

    #[test]
    fn declaration() -> Result<()> {
        let xml = r#"<!DOCTYPE example>"#;
        let mut tester = TestRecv::with_bytes(xml);
        let mut eval = RecvEvaluator::new();
        let declaration = Declaration::from(r#"<!DOCTYPE example>"#.as_bytes());
        assert!(tester.assert_next_token(&mut eval, Some(Token::Declaration(declaration))));
        assert_eq!(eval.position(), 18);
        assert!(tester.assert_next_token(&mut eval, Some(Token::Eof)));
        assert_eq!(eval.position(), 18);
        assert!(tester.assert_next_token(&mut eval, None));
        assert_eq!(eval.position(), 18);
        Ok(())
    }

    #[test]
    fn comment() -> Result<()> {
        let xml = r#"<!-- Example -->"#;
        let mut tester = TestRecv::with_bytes(xml);
        let mut eval = RecvEvaluator::new();
        let comment = Comment::from(r#"<!-- Example -->"#.as_bytes());
        assert!(tester.assert_next_token(&mut eval, Some(Token::Comment(comment))));
        assert_eq!(eval.position(), 16);
        assert!(tester.assert_next_token(&mut eval, Some(Token::Eof)));
        assert_eq!(eval.position(), 16);
        assert!(tester.assert_next_token(&mut eval, None));
        assert_eq!(eval.position(), 16);
        Ok(())
    }

    #[test]
    fn cdata() -> Result<()> {
        let xml = r#"<![CDATA[ <Example> ]]>"#;
        let mut tester = TestRecv::with_bytes(xml);
        let mut eval = RecvEvaluator::new();
        let cdata = Cdata::from(r#"<![CDATA[ <Example> ]]>"#.as_bytes());
        assert!(tester.assert_next_token(&mut eval, Some(Token::Cdata(cdata))));
        assert_eq!(eval.position(), 23);
        assert!(tester.assert_next_token(&mut eval, Some(Token::Eof)));
        assert_eq!(eval.position(), 23);
        assert!(tester.assert_next_token(&mut eval, None));
        assert_eq!(eval.position(), 23);
        Ok(())
    }

    #[test]
    fn bytes_not_evaluated() -> Result<()> {
        let xml = r#"<unfinished name="xml""#;
        let mut tester = TestRecv::with_bytes(xml);
        let mut eval = RecvEvaluator::new();
        let bytes_not_evaluated = BytesNotEvaluated::from(r#"<unfinished name="xml""#.as_bytes());
        assert!(tester.assert_next_token(
            &mut eval,
            Some(Token::EofWithBytesNotEvaluated(bytes_not_evaluated))
        ));
        assert_eq!(eval.position(), 22);
        assert!(tester.assert_next_token(&mut eval, None));
        assert_eq!(eval.position(), 22);
        Ok(())
    }

    #[test]
    fn simple_xml_read() -> Result<()> {
        let xml = r#"<hello name="rust">Welcome!<goodbye/></hello><abcd></abcd>"#;
        let mut tester = TestRecv::with_bytes(xml);
        let mut eval = RecvEvaluator::new();
        let start_tag = StartTag::from(r#"<hello name="rust">"#.as_bytes());
        assert!(tester.assert_next_token(&mut eval, Some(Token::StartTag(start_tag))));
        assert_eq!(eval.position(), 19);

        let text_content = Characters::from(r#"Welcome!"#.as_bytes());
        assert!(tester.assert_next_token(&mut eval, Some(Token::Characters(text_content))));
        assert_eq!(eval.position(), 27);

        let empty_element_tag = EmptyElementTag::from(r#"<goodbye/>"#.as_bytes());
        assert!(
            tester.assert_next_token(&mut eval, Some(Token::EmptyElementTag(empty_element_tag)))
        );
        assert_eq!(eval.position(), 37);

        let end_tag = EndTag::from(r#"</hello>"#.as_bytes());
        assert!(tester.assert_next_token(&mut eval, Some(Token::EndTag(end_tag))));
        assert_eq!(eval.position(), 45);

        let start_tag = StartTag::from(r#"<abcd>"#.as_bytes());
        assert!(tester.assert_next_token(&mut eval, Some(Token::StartTag(start_tag))));
        assert_eq!(eval.position(), 51);

        let end_tag = EndTag::from(r#"</abcd>"#.as_bytes());
        assert!(tester.assert_next_token(&mut eval, Some(Token::EndTag(end_tag))));
        assert_eq!(eval.position(), 58);

        assert!(tester.assert_next_token(&mut eval, Some(Token::Eof)));
        assert_eq!(eval.position(), 58);

        assert!(tester.assert_next_token(&mut eval, None));
        assert_eq!(eval.position(), 58);

        Ok(())
    }

    #[test]
    fn simple_xml_read_with_space() -> Result<()> {
        let xml = r#"   <hello name="rust"> Welcome! <goodbye  />  </hello>  <abcd> </abcd> "#;
        let mut tester = TestRecv::with_bytes(xml);
        let mut eval = RecvEvaluator::new();

        let text_content = Characters::from(b"   ".as_ref());
        assert!(tester.assert_next_token(&mut eval, Some(Token::Characters(text_content))));
        assert_eq!(eval.position(), 3);

        let start_tag = StartTag::from(r#"<hello name="rust">"#.as_bytes());
        assert!(tester.assert_next_token(&mut eval, Some(Token::StartTag(start_tag))));
        assert_eq!(eval.position(), 22);

        let text_content = Characters::from(b" Welcome! ".as_ref());
        assert!(tester.assert_next_token(&mut eval, Some(Token::Characters(text_content))));
        assert_eq!(eval.position(), 32);

        let empty_element_tag = EmptyElementTag::from(r#"<goodbye  />"#.as_bytes());
        assert!(
            tester.assert_next_token(&mut eval, Some(Token::EmptyElementTag(empty_element_tag)))
        );
        assert_eq!(eval.position(), 44);

        let text_content = Characters::from(b"  ".as_ref());
        assert!(tester.assert_next_token(&mut eval, Some(Token::Characters(text_content))));
        assert_eq!(eval.position(), 46);

        let end_tag = EndTag::from(r#"</hello>"#.as_bytes());
        assert!(tester.assert_next_token(&mut eval, Some(Token::EndTag(end_tag))));
        assert_eq!(eval.position(), 54);

        let text_content = Characters::from(b"  ".as_ref());
        assert!(tester.assert_next_token(&mut eval, Some(Token::Characters(text_content))));
        assert_eq!(eval.position(), 56);

        let start_tag = StartTag::from(r#"<abcd>"#.as_bytes());
        assert!(tester.assert_next_token(&mut eval, Some(Token::StartTag(start_tag))));
        assert_eq!(eval.position(), 62);

        let text_content = Characters::from(b" ".as_ref());
        assert!(tester.assert_next_token(&mut eval, Some(Token::Characters(text_content))));
        assert_eq!(eval.position(), 63);

        let end_tag = EndTag::from(r#"</abcd>"#.as_bytes());
        assert!(tester.assert_next_token(&mut eval, Some(Token::EndTag(end_tag))));
        assert_eq!(eval.position(), 70);

        let text_content = Characters::from(b" ".as_ref());
        assert!(tester.assert_next_token(&mut eval, Some(Token::Characters(text_content))));
        assert_eq!(eval.position(), 71);

        assert!(tester.assert_next_token(&mut eval, Some(Token::Eof)));
        assert_eq!(eval.position(), 71);

        assert!(tester.assert_next_token(&mut eval, None));
        assert_eq!(eval.position(), 71);

        Ok(())
    }
}
