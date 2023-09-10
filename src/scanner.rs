// Copyright 2022 Bryant Luk
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Scans byte sequences for tokens.

use crate::bytes::{
    self, AlreadyFoundByteSeqCount, BracketCount, QuoteAndBracketContextAwareFoundState,
    QuoteContextAwareFoundState, QuoteState,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Offset(usize);

/// What the scanner is working on.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum InternalState {
    Reset,
    ScanningMarkup,
    ScanningStartOrEmptyElementTag(QuoteState, bool),
    ScanningEndTag(QuoteState),
    ScanningCharacters,
    ScanningProcessingInstruction(bool),
    ScanningDeclarationCommentOrCdata([u8; 7], usize),
    ScanningDeclaration(QuoteState, BracketCount, AlreadyFoundByteSeqCount),
    ScanningComment(AlreadyFoundByteSeqCount),
    ScanningCdata(AlreadyFoundByteSeqCount),
    Eof,
}

/// What the scanner has seen.
///
/// The variants which begin with `Scanning` indicate that the scanner has not
/// found the terminating delimiters in the current scanned byte slice. The calling
/// code to the scanner should consume the entire byte slice. For instance, if the
/// calling code is interested in the contents, the entire byte slice should be
/// stored into a buffer.
///
/// On the other hand, the variants which begin with `Scanned` indicate that the
/// scanner has found the terminating delimiters for the current token. The `usize`
/// associated value indicate the number of bytes read to find the terminating
/// delimiters in the current scanned byte slice. The calling code should
/// consume `bytes[..read]`.
///
/// ```
/// # use maybe_xml::scanner::{State, Scanner};
/// let mut scanner = Scanner::new();
/// let mut buffer = Vec::new();
/// let bytes_to_scan = b"<h";
/// let state = scanner.scan(bytes_to_scan);
/// assert_eq!(state, Some(State::ScanningStartOrEmptyElementTag));
/// buffer.extend_from_slice(bytes_to_scan);
///
/// let bytes_to_scan = b"el";
/// let state = scanner.scan(bytes_to_scan);
/// assert_eq!(state, Some(State::ScanningStartOrEmptyElementTag));
/// buffer.extend_from_slice(bytes_to_scan);
///
/// let bytes_to_scan = b"lo>Other Content";
/// let state = scanner.scan(bytes_to_scan);
/// assert_eq!(state, Some(State::ScannedStartTag(3)));
/// buffer.extend_from_slice(&bytes_to_scan[..3]);
///
/// assert_eq!(buffer, b"<hello>");
/// ```
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum State {
    /// Characters in-between markup has been scanned but no terminating markup or end of file has been scanned.
    ScanningCharacters,
    /// Characters in-between markup has been scanned.
    ScannedCharacters(usize),
    /// A `<` was scanned, but nothing else.
    ScanningMarkup,
    /// A `<` was scanned with at least the start of an element name.
    ScanningStartOrEmptyElementTag,
    /// A start tag like `<greeting>` was scanned.
    ScannedStartTag(usize),
    /// An empty tag like `<br/>` was scanned.
    ScannedEmptyElementTag(usize),
    /// A `</` was scanned.
    ScanningEndTag,
    /// An end tag like `</greeting>` was scanned.
    ScannedEndTag(usize),
    /// A `<!` was scanned but not enough to determine if it is a declaration, comment, or CDATA.
    ScanningDeclarationCommentOrCdata,
    /// A declaration has been scanned but no terminating `>` has been scanned.
    ScanningDeclaration,
    /// A declaration like `<!ENTITY>` was scanned.
    ScannedDeclaration(usize),
    /// A comment has been scanned but no terminating `-->` has been scanned.
    ScanningComment,
    /// A comment like `<!-- Todo -->` was scanned.
    ScannedComment(usize),
    /// A `<![CDATA[` was scanned but the terminating `]]>` was not scanned.
    ScanningCdata,
    /// Character data like `<![CDATA[ Testing ]]>` was scanned.
    ScannedCdata(usize),
    /// A `<?` was scanned.
    ScanningProcessingInstruction,
    /// A processing instruction like `<?validator option="true"?>` was scanned.
    ScannedProcessingInstruction(usize),
}

/// Stores the state of the current token being scanned and scans byte sequences
/// for when the current token ends.
///
/// The `Scanner` does not copy the bytes being scanned. The internal state only
/// contains data to determine when the current token has ended.
///
/// # Important
///
/// The `Scanner` is meant to be used to scan a stream of bytes split into byte
/// slice parameters. An empty byte slice indicates the end of the byte stream.
/// After an empty byte slice is given to the `scan` method, the `scan` method
/// will always return `None`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scanner {
    /// What the scanner is working on.
    state: InternalState,
}

impl Scanner {
    /// Instantiates a new scanner for a new byte stream.
    #[must_use]
    pub fn new() -> Self {
        Self {
            state: InternalState::Reset,
        }
    }

    #[inline]
    fn scan_markup(&mut self, bytes: &[u8]) -> Option<State> {
        if let Some(next) = bytes::peek2(bytes) {
            match next {
                b'/' => self.scan_end_tag(bytes, QuoteState::None, Offset(2)),
                b'?' => self.scan_processing_instruction(bytes, false, Offset(2)),
                b'!' => self.scan_declaration_comment_or_cdata(bytes, [0; 7], 0, Offset(2)),
                _ => {
                    self.scan_start_or_empty_element_tag(bytes, QuoteState::None, false, Offset(1))
                }
            }
        } else {
            self.state = InternalState::ScanningMarkup;
            debug_assert_eq!(bytes.len(), 1);
            Some(State::ScanningMarkup)
        }
    }

    #[inline]
    fn scan_markup2(&mut self, bytes: &[u8]) -> Option<State> {
        if let Some(next) = bytes::peek(bytes) {
            match next {
                b'/' => self.scan_end_tag(bytes, QuoteState::None, Offset(1)),
                b'?' => self.scan_processing_instruction(bytes, false, Offset(1)),
                b'!' => self.scan_declaration_comment_or_cdata(bytes, [0; 7], 0, Offset(1)),
                _ => {
                    self.scan_start_or_empty_element_tag(bytes, QuoteState::None, false, Offset(0))
                }
            }
        } else {
            self.state = InternalState::Eof;
            None
        }
    }

    fn scan_start_or_empty_element_tag(
        &mut self,
        bytes: &[u8],
        quote_state: QuoteState,
        is_last_char_slash: bool,
        offset: Offset,
    ) -> Option<State> {
        if bytes.is_empty() {
            self.state = InternalState::Eof;
            return None;
        }

        let found = bytes::quote_context_aware_find(&bytes[(offset.0)..], quote_state);

        match found {
            QuoteContextAwareFoundState::Found(read) => {
                self.state = InternalState::Reset;

                if read > 1 && bytes[offset.0 + read - 2] == b'/' {
                    Some(State::ScannedEmptyElementTag(offset.0 + read))
                } else if is_last_char_slash && read == 1 {
                    debug_assert_eq!(offset.0, 0);
                    Some(State::ScannedEmptyElementTag(read))
                } else {
                    Some(State::ScannedStartTag(offset.0 + read))
                }
            }
            QuoteContextAwareFoundState::NotFound(quote_state) => {
                let last_char_slash = match quote_state {
                    QuoteState::None => bytes.last() == Some(&b'/'),
                    QuoteState::Single | QuoteState::Double => false,
                };
                self.state =
                    InternalState::ScanningStartOrEmptyElementTag(quote_state, last_char_slash);
                Some(State::ScanningStartOrEmptyElementTag)
            }
        }
    }

    fn scan_end_tag(
        &mut self,
        bytes: &[u8],
        quote_state: QuoteState,
        offset: Offset,
    ) -> Option<State> {
        if bytes.is_empty() {
            self.state = InternalState::Eof;
            return None;
        }

        let found = bytes::quote_context_aware_find(&bytes[(offset.0)..], quote_state);
        match found {
            QuoteContextAwareFoundState::Found(read) => {
                self.state = InternalState::Reset;
                Some(State::ScannedEndTag(offset.0 + read))
            }
            QuoteContextAwareFoundState::NotFound(quote_state) => {
                self.state = InternalState::ScanningEndTag(quote_state);
                Some(State::ScanningEndTag)
            }
        }
    }

    fn scan_processing_instruction(
        &mut self,
        bytes: &[u8],
        question_mark_was_seen: bool,
        offset: Offset,
    ) -> Option<State> {
        if bytes.is_empty() {
            self.state = InternalState::Eof;
            return None;
        }

        if question_mark_was_seen {
            if bytes.get(offset.0) == Some(&b'>') {
                self.state = InternalState::Reset;
                return Some(State::ScannedProcessingInstruction(offset.0 + 1));
            }
        }

        let mut bytes_to_search = &bytes[offset.0..];
        let mut read = 0;

        loop {
            if let Some(index) = bytes_to_search.iter().position(|b| *b == b'>') {
                let end = index + 1;
                read += end;

                if index > 0 && bytes_to_search[index - 1] == b'?' {
                    self.state = InternalState::Reset;
                    return Some(State::ScannedProcessingInstruction(offset.0 + read));
                }

                bytes_to_search = &bytes_to_search[end..];
            } else {
                let was_seen = bytes[offset.0..].last().map_or(false, |b| *b == b'?');
                self.state = InternalState::ScanningProcessingInstruction(was_seen);
                return Some(State::ScanningProcessingInstruction);
            }
        }
    }

    fn scan_declaration_comment_or_cdata(
        &mut self,
        bytes: &[u8],
        mut filled_array: [u8; 7],
        mut filled_count: usize,
        offset: Offset,
    ) -> Option<State> {
        if bytes.is_empty() {
            self.state = InternalState::Eof;
            return None;
        }

        let bytes_to_check = &bytes[(offset.0)..];
        let cdata = b"[CDATA[";

        let to_fill = usize::min(filled_array.len() - filled_count, bytes_to_check.len());
        if to_fill > 0 {
            filled_array[filled_count..to_fill + filled_count]
                .copy_from_slice(&bytes_to_check[..to_fill]);
        }
        filled_count += to_fill;

        if filled_count > 0 {
            match filled_array[0] {
                b'-' => {
                    if filled_count > 1 {
                        match filled_array[1] {
                            b'-' => self.scan_comment(
                                bytes,
                                AlreadyFoundByteSeqCount(0),
                                Offset(offset.0 + usize::min(to_fill, 2)),
                            ),
                            _ => self.scan_declaration(
                                bytes,
                                QuoteState::None,
                                BracketCount(0),
                                AlreadyFoundByteSeqCount(0),
                                Offset(offset.0),
                            ),
                        }
                    } else {
                        debug_assert_eq!(filled_array[0], b'-');
                        debug_assert_eq!(filled_count, 1);
                        self.state = InternalState::ScanningDeclarationCommentOrCdata(
                            filled_array,
                            filled_count,
                        );
                        Some(State::ScanningDeclarationCommentOrCdata)
                    }
                }
                b'[' => {
                    if filled_array[..filled_count] == cdata[..filled_count] {
                        if filled_count == 7 {
                            self.scan_cdata(
                                bytes,
                                AlreadyFoundByteSeqCount(0),
                                Offset(offset.0 + to_fill),
                            )
                        } else {
                            self.state = InternalState::ScanningDeclarationCommentOrCdata(
                                filled_array,
                                filled_count,
                            );
                            Some(State::ScanningDeclarationCommentOrCdata)
                        }
                    } else {
                        let mut bracket_count: u64 = 0;
                        for byte in &filled_array[..filled_count - to_fill] {
                            match byte {
                                b'[' => {
                                    bracket_count += 1;
                                }
                                b']' => {
                                    bracket_count = bracket_count.saturating_sub(1);
                                }
                                _ => {}
                            }
                        }
                        self.scan_declaration(
                            bytes,
                            QuoteState::None,
                            BracketCount(bracket_count),
                            AlreadyFoundByteSeqCount(0),
                            Offset(offset.0),
                        )
                    }
                }
                _ => self.scan_declaration(
                    bytes,
                    QuoteState::None,
                    BracketCount(0),
                    AlreadyFoundByteSeqCount(0),
                    Offset(offset.0),
                ),
            }
        } else {
            self.state = InternalState::ScanningDeclarationCommentOrCdata([0; 7], 0);
            Some(State::ScanningDeclarationCommentOrCdata)
        }
    }

    fn scan_declaration(
        &mut self,
        bytes: &[u8],
        quote_state: QuoteState,
        bracket_count: BracketCount,
        already_found_byte_seq_count: AlreadyFoundByteSeqCount,
        offset: Offset,
    ) -> Option<State> {
        if bytes.is_empty() {
            self.state = InternalState::Eof;
            return None;
        }

        let byte_seq = b">";
        let found = bytes::quote_and_bracket_context_aware_find(
            &bytes[(offset.0)..],
            byte_seq,
            already_found_byte_seq_count,
            quote_state,
            bracket_count,
        );
        match found {
            QuoteAndBracketContextAwareFoundState::Found(read) => {
                self.state = InternalState::Reset;
                Some(State::ScannedDeclaration(offset.0 + read))
            }
            QuoteAndBracketContextAwareFoundState::NotFound(
                quote_state,
                bracket_count,
                already_found_byte_seq_count,
            ) => {
                self.state = InternalState::ScanningDeclaration(
                    quote_state,
                    bracket_count,
                    already_found_byte_seq_count,
                );
                Some(State::ScanningDeclaration)
            }
        }
    }

    fn scan_comment(
        &mut self,
        bytes: &[u8],
        already_found_byte_seq_count: AlreadyFoundByteSeqCount,
        offset: Offset,
    ) -> Option<State> {
        if bytes.is_empty() {
            self.state = InternalState::Eof;
            return None;
        }

        if already_found_byte_seq_count.0 > 0 {
            match already_found_byte_seq_count.0 {
                1 => {
                    if bytes.get(offset.0) == Some(&b'-') {
                        match bytes.get(offset.0 + 1) {
                            Some(&b'>') => {
                                self.state = InternalState::Reset;
                                return Some(State::ScannedComment(offset.0 + 2));
                            }
                            None => {
                                self.state =
                                    InternalState::ScanningComment(AlreadyFoundByteSeqCount(2));
                                return Some(State::ScanningComment);
                            }
                            _ => {}
                        }
                    }
                }
                2 => {
                    if bytes.get(offset.0) == Some(&b'>') {
                        self.state = InternalState::Reset;
                        return Some(State::ScannedComment(offset.0 + 1));
                    }
                }
                _ => unreachable!("should only match up to 2"),
            }
        }

        let mut bytes_to_search = &bytes[offset.0..];
        let mut read = 0;

        loop {
            if let Some(index) = bytes_to_search.iter().position(|b| *b == b'>') {
                let end = index + 1;
                read += end;

                if index > 1 && &bytes_to_search[index - 2..end] == b"-->" {
                    self.state = InternalState::Reset;
                    return Some(State::ScannedComment(offset.0 + read));
                }

                bytes_to_search = &bytes_to_search[end..];
            } else {
                let already_found_byte_seq_count =
                    bytes::find_matching_suffix(b"-->", &bytes[offset.0..]);
                self.state = InternalState::ScanningComment(already_found_byte_seq_count);
                return Some(State::ScanningComment);
            }
        }
    }

    fn scan_cdata(
        &mut self,
        bytes: &[u8],
        already_found_byte_seq_count: AlreadyFoundByteSeqCount,
        offset: Offset,
    ) -> Option<State> {
        if bytes.is_empty() {
            self.state = InternalState::Eof;
            return None;
        }

        if already_found_byte_seq_count.0 > 0 {
            match already_found_byte_seq_count.0 {
                1 => {
                    if bytes.get(offset.0) == Some(&b']') {
                        match bytes.get(offset.0 + 1) {
                            Some(&b'>') => {
                                self.state = InternalState::Reset;
                                return Some(State::ScannedCdata(offset.0 + 2));
                            }
                            None => {
                                self.state =
                                    InternalState::ScanningCdata(AlreadyFoundByteSeqCount(2));
                                return Some(State::ScanningCdata);
                            }
                            _ => {}
                        }
                    }
                }
                2 => {
                    if bytes.get(offset.0) == Some(&b'>') {
                        self.state = InternalState::Reset;
                        return Some(State::ScannedCdata(offset.0 + 1));
                    }
                }
                _ => unreachable!("should only match up to 2"),
            }
        }

        let mut bytes_to_search = &bytes[offset.0..];
        let mut read = 0;

        loop {
            if let Some(index) = bytes_to_search.iter().position(|b| *b == b'>') {
                let end = index + 1;
                read += end;

                if index > 1 && &bytes_to_search[index - 2..end] == b"]]>" {
                    self.state = InternalState::Reset;
                    return Some(State::ScannedCdata(offset.0 + read));
                }

                bytes_to_search = &bytes_to_search[end..];
            } else {
                let already_found_byte_seq_count =
                    bytes::find_matching_suffix(b"]]>", &bytes[offset.0..]);
                self.state = InternalState::ScanningCdata(already_found_byte_seq_count);
                return Some(State::ScanningCdata);
            }
        }
    }

    fn scan_text_content(&mut self, bytes: &[u8]) -> State {
        if bytes.is_empty() {
            self.state = InternalState::Eof;
            // reached this state because there was previous text content
            // so return a "finished" state even though normally this should be `None`
            State::ScannedCharacters(0)
        } else if let Some(index) = bytes.iter().position(|b| *b == b'<') {
            self.state = InternalState::Reset;
            State::ScannedCharacters(index)
        } else {
            self.state = InternalState::ScanningCharacters;
            State::ScanningCharacters
        }
    }

    /// Scans the byte slice up to the end of the current token.
    ///
    /// The scanner does not copy or store the contents of the parameter. When a
    /// `State` variant is returned which begins with `Scanning`, then the entire
    /// byte slice parameter should be consumed by the calling code. In effect,
    /// the `Scanning` variants signal that either a new token is starting to be
    /// scanned or the (unfinished) current token is continuing to be scanned. In
    /// either case, the terminating delimiters for the token were not found in
    /// the byte slice parameter.
    ///
    /// When a `State` variant is returned which begins with `Scanned`, it has an
    /// usize associated value. The usize associated value is the exclusive index
    /// value which should be consumed. In other words, if a variant like
    /// `ScannedEndTag(read)` is returned from this function, the
    /// `&bytes[..read]` should be consumed. In effect, either a new token was
    /// completely scanned or the (previously unfinished) current token was
    /// finished being scanned. In either case, the relevant content for the
    /// token is contained in `&bytes[..read]`.
    ///
    /// Any bytes unconsumed (e.g. `&bytes[read..]`) should be passed to
    /// the `scan` function again. The scanner only scans one token at a time.
    ///
    /// # Important
    ///
    /// If an empty byte slice is given to `scan`, the empty byte slice informs
    /// the scanner that the "end of file" marker has been reached. `None` will
    /// always be returned from the scanner after the `scan` method is given
    /// the empty byte slice, even if future byte slices are non-empty.
    ///
    /// ```
    /// # use maybe_xml::{token::borrowed::{StartTag, Characters, EndTag, Token}, scanner::{State, Scanner}};
    /// # use std::io::BufRead;
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
    /// let mut buf = std::io::BufReader::new(r#"<hello>world!</hello>"#.as_bytes());
    ///
    /// let mut scanner = Scanner::new();
    ///
    /// let bytes_to_read = buf.fill_buf()?;
    /// assert_eq!(bytes_to_read, r#"<hello>world!</hello>"#.as_bytes());
    /// let state = scanner.scan(&bytes_to_read);
    /// if let Some(State::ScannedStartTag(read)) = state {
    ///     let start_tag = StartTag::from(&bytes_to_read[..read]);
    ///     assert_eq!(start_tag.name().to_str()?, "hello");
    ///     buf.consume(read);
    /// } else {
    ///     assert!(false, "should have scanned bytes");
    /// }
    ///
    /// let bytes_to_read = buf.fill_buf()?;
    /// assert_eq!(bytes_to_read, r#"world!</hello>"#.as_bytes());
    /// let state = scanner.scan(&bytes_to_read);
    /// if let Some(State::ScannedCharacters(read)) = state {
    ///     let characters = Characters::from(&bytes_to_read[..read]);
    ///     assert_eq!(characters .to_str()?, "world!");
    ///     buf.consume(read);
    /// } else {
    ///     assert!(false, "should have scanned bytes");
    /// }
    ///
    /// let bytes_to_read = buf.fill_buf()?;
    /// assert_eq!(bytes_to_read, r#"</hello>"#.as_bytes());
    /// let state = scanner.scan(&bytes_to_read);
    /// if let Some(State::ScannedEndTag(read)) = state {
    ///     let end_tag = EndTag::from(&bytes_to_read[..read]);
    ///     assert_eq!(end_tag.to_str()?, "</hello>");
    ///     buf.consume(read);
    /// } else {
    ///     assert!(false, "should have scanned bytes");
    /// }
    ///
    /// let bytes_to_read = buf.fill_buf()?;
    /// assert_eq!(bytes_to_read, r#""#.as_bytes());
    /// let state = scanner.scan(&bytes_to_read);
    /// assert_eq!(state, None);
    /// # Ok(())
    /// # }
    /// ```
    pub fn scan(&mut self, bytes: &[u8]) -> Option<State> {
        match self.state {
            InternalState::Reset => match bytes::peek(bytes) {
                None => {
                    self.state = InternalState::Eof;
                    None
                }
                Some(b'<') => self.scan_markup(bytes),
                Some(_) => Some(self.scan_text_content(bytes)),
            },
            InternalState::ScanningMarkup => self.scan_markup2(bytes),
            InternalState::ScanningStartOrEmptyElementTag(quote_state, is_last_char_slash) => self
                .scan_start_or_empty_element_tag(bytes, quote_state, is_last_char_slash, Offset(0)),
            InternalState::ScanningEndTag(quote_state) => {
                self.scan_end_tag(bytes, quote_state, Offset(0))
            }
            InternalState::ScanningCharacters => Some(self.scan_text_content(bytes)),
            InternalState::ScanningProcessingInstruction(already_found_byte_seq_count) => {
                self.scan_processing_instruction(bytes, already_found_byte_seq_count, Offset(0))
            }
            InternalState::ScanningDeclarationCommentOrCdata(filled_array, filled_count) => {
                self.scan_declaration_comment_or_cdata(bytes, filled_array, filled_count, Offset(0))
            }
            InternalState::ScanningDeclaration(
                quote_state,
                bracket_count,
                already_found_byte_seq_count,
            ) => self.scan_declaration(
                bytes,
                quote_state,
                bracket_count,
                already_found_byte_seq_count,
                Offset(0),
            ),
            InternalState::ScanningComment(already_found_byte_seq_count) => {
                self.scan_comment(bytes, already_found_byte_seq_count, Offset(0))
            }
            InternalState::ScanningCdata(already_found_byte_seq_count) => {
                self.scan_cdata(bytes, already_found_byte_seq_count, Offset(0))
            }
            InternalState::Eof => None,
        }
    }
}

impl Default for Scanner {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn none_on_empty() {
        let mut scanner = Scanner::new();
        assert_eq!(scanner.state, InternalState::Reset);
        assert_eq!(scanner.scan(b""), None);
        assert_eq!(scanner.state, InternalState::Eof);
        assert_eq!(scanner.scan(b"<hello>"), None);
        assert_eq!(scanner.state, InternalState::Eof);
    }

    #[test]
    fn text_content() {
        let mut scanner = Scanner::new();
        let bytes = r"Hello".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningCharacters));
        assert_eq!(scanner.state, InternalState::ScanningCharacters);
        let bytes = r"wo".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningCharacters));
        let bytes = r"rld!<".as_bytes();
        assert_eq!(scanner.state, InternalState::ScanningCharacters);
        assert_eq!(scanner.scan(bytes), Some(State::ScannedCharacters(4)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn text_content_finish_on_empty_bytes() {
        let mut scanner = Scanner::new();
        let bytes = r"Hello".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningCharacters));
        assert_eq!(scanner.state, InternalState::ScanningCharacters);
        let bytes = r"".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedCharacters(0)));
        assert_eq!(scanner.state, InternalState::Eof);
        assert_eq!(scanner.scan(b"<hello>"), None);
        assert_eq!(scanner.state, InternalState::Eof);
    }

    #[test]
    fn start_of_markup() {
        let mut scanner = Scanner::new();
        let bytes = r"<".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
        assert_eq!(scanner.state, InternalState::ScanningMarkup);
    }

    #[test]
    fn start_of_markup_eof() {
        let mut scanner = Scanner::new();
        let bytes = r"<".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
        assert_eq!(scanner.state, InternalState::ScanningMarkup);
        let bytes = r"".as_bytes();
        assert_eq!(scanner.scan(bytes), None);
        assert_eq!(scanner.state, InternalState::Eof);
        assert_eq!(scanner.scan(b"<hello>"), None);
        assert_eq!(scanner.state, InternalState::Eof);
    }

    #[test]
    fn start_tag_in_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r"<hello>".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedStartTag(7)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn start_tag_eof() {
        let mut scanner = Scanner::new();
        let bytes = r"<hello".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningStartOrEmptyElementTag)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningStartOrEmptyElementTag(QuoteState::None, false)
        );
        let bytes = r"".as_bytes();
        assert_eq!(scanner.scan(bytes), None);
        assert_eq!(scanner.state, InternalState::Eof);
        assert_eq!(scanner.scan(b"<hello>"), None);
        assert_eq!(scanner.state, InternalState::Eof);
    }

    #[test]
    fn start_tag_with_only_markup_in_first_part() {
        let mut scanner = Scanner::new();
        let bytes = r"<".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
        assert_eq!(scanner.state, InternalState::ScanningMarkup);

        let bytes = r"hello".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningStartOrEmptyElementTag)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningStartOrEmptyElementTag(QuoteState::None, false)
        );

        let bytes = r">Content".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedStartTag(1)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn start_tag_in_parts() {
        let mut scanner = Scanner::new();
        let bytes = r"<hello".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningStartOrEmptyElementTag)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningStartOrEmptyElementTag(QuoteState::None, false)
        );

        let bytes = r">Some content".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedStartTag(1)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn start_tag_with_single_quotes_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r#"<hello a='val>'>Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedStartTag(16)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn start_tag_with_single_quotes_in_parts() {
        let mut scanner = Scanner::new();
        let bytes = r#"<hello a='"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningStartOrEmptyElementTag)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningStartOrEmptyElementTag(QuoteState::Single, false)
        );

        let bytes = r#"val>'>Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedStartTag(6)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn start_tag_with_double_quotes_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r#"<hello a="val>">Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedStartTag(16)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn start_tag_with_double_quotes_in_parts() {
        let mut scanner = Scanner::new();
        let bytes = r#"<hello a=""#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningStartOrEmptyElementTag)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningStartOrEmptyElementTag(QuoteState::Double, false)
        );

        let bytes = r#"val>">Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedStartTag(6)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn empty_element_tag_in_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r"<hello/>".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedEmptyElementTag(8)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn empty_element_tag_eof() {
        let mut scanner = Scanner::new();
        let bytes = r"<hello/".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningStartOrEmptyElementTag)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningStartOrEmptyElementTag(QuoteState::None, true)
        );
        let bytes = r"".as_bytes();
        assert_eq!(scanner.scan(bytes), None);
        assert_eq!(scanner.state, InternalState::Eof);
        assert_eq!(scanner.scan(b">"), None);
        assert_eq!(scanner.state, InternalState::Eof);
    }

    #[test]
    fn empty_element_tag_with_slash_in_standalone_part() {
        let mut scanner = Scanner::new();
        let bytes = r"<".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
        assert_eq!(scanner.state, InternalState::ScanningMarkup);

        let bytes = r"hello".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningStartOrEmptyElementTag)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningStartOrEmptyElementTag(QuoteState::None, false)
        );

        let bytes = r"/".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningStartOrEmptyElementTag)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningStartOrEmptyElementTag(QuoteState::None, true)
        );

        let bytes = r">Content".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedEmptyElementTag(1)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn empty_element_tag_with_only_markup_in_first_part() {
        let mut scanner = Scanner::new();
        let bytes = r"<".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
        assert_eq!(scanner.state, InternalState::ScanningMarkup);

        let bytes = r"hello".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningStartOrEmptyElementTag)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningStartOrEmptyElementTag(QuoteState::None, false)
        );

        let bytes = r"/>Content".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedEmptyElementTag(2)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn empty_element_tag_with_double_quotes_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r#"<hello a="val/>"/>Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedEmptyElementTag(18)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn empty_element_tag_with_double_quotes_in_parts() {
        let mut scanner = Scanner::new();
        let bytes = r#"<hello a=""#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningStartOrEmptyElementTag)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningStartOrEmptyElementTag(QuoteState::Double, false)
        );

        let bytes = r#"val/>"/>Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedEmptyElementTag(8)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn empty_element_tag_last_slash_split_across_parts() {
        let mut scanner = Scanner::new();
        let bytes = r#"<hello/"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningStartOrEmptyElementTag)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningStartOrEmptyElementTag(QuoteState::None, true)
        );

        let bytes = r#">Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedEmptyElementTag(1)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn empty_element_tag_last_slash_split_across_parts_with_single_quotes() {
        let mut scanner = Scanner::new();
        let bytes = r#"<hello attr='/"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningStartOrEmptyElementTag)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningStartOrEmptyElementTag(QuoteState::Single, false)
        );

        let bytes = r#">'/>Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedEmptyElementTag(4)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn empty_element_tag_last_slash_split_across_parts_with_double_quotes() {
        let mut scanner = Scanner::new();
        let bytes = r#"<hello attr="/"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningStartOrEmptyElementTag)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningStartOrEmptyElementTag(QuoteState::Double, false)
        );

        let bytes = r#">"/>Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedEmptyElementTag(4)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn start_tag_last_slash_split_across_parts() {
        let mut scanner = Scanner::new();
        let bytes = r#"<hello/"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningStartOrEmptyElementTag)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningStartOrEmptyElementTag(QuoteState::None, true)
        );

        let bytes = r#" invalid>Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedStartTag(9)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn end_tag_in_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r"</goodbye>".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedEndTag(10)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn end_tag_eof() {
        let mut scanner = Scanner::new();
        let bytes = r"</hello".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningEndTag));
        assert_eq!(
            scanner.state,
            InternalState::ScanningEndTag(QuoteState::None)
        );
        let bytes = r"".as_bytes();
        assert_eq!(scanner.scan(bytes), None);
        assert_eq!(scanner.state, InternalState::Eof);
        assert_eq!(scanner.scan(b">"), None);
        assert_eq!(scanner.state, InternalState::Eof);
    }

    #[test]
    fn end_tag_with_only_markup_in_first_part() {
        let mut scanner = Scanner::new();
        let bytes = r"<".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
        assert_eq!(scanner.state, InternalState::ScanningMarkup);

        let bytes = r"/goodbye".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningEndTag));
        assert_eq!(
            scanner.state,
            InternalState::ScanningEndTag(QuoteState::None)
        );

        let bytes = r">Content".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedEndTag(1)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn end_tag_with_slash_as_only_part() {
        let mut scanner = Scanner::new();
        let bytes = r"<".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
        assert_eq!(scanner.state, InternalState::ScanningMarkup);

        let bytes = r"/".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningEndTag));
        assert_eq!(
            scanner.state,
            InternalState::ScanningEndTag(QuoteState::None)
        );

        let bytes = r"goodbye>Content".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedEndTag(8)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn end_tag_in_parts() {
        let mut scanner = Scanner::new();
        let bytes = r"</goodbye".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningEndTag));
        assert_eq!(
            scanner.state,
            InternalState::ScanningEndTag(QuoteState::None)
        );

        let bytes = r">Some content".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedEndTag(1)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn end_tag_with_single_quotes_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r#"</goodbye a='val>'>Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedEndTag(19)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn end_tag_with_single_quotes_in_parts() {
        let mut scanner = Scanner::new();
        let bytes = r#"</goodbye a='"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningEndTag));
        assert_eq!(
            scanner.state,
            InternalState::ScanningEndTag(QuoteState::Single)
        );

        let bytes = r#"val>'>Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedEndTag(6)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn end_tag_with_double_quotes_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r#"</goodbye a="val>">Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedEndTag(19)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn end_tag_with_double_quotes_in_parts() {
        let mut scanner = Scanner::new();
        let bytes = r#"</goodbye a=""#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningEndTag));
        assert_eq!(
            scanner.state,
            InternalState::ScanningEndTag(QuoteState::Double)
        );

        let bytes = r#"val>">Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedEndTag(6)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn pi_in_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r#"<?test a="b" ?>"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScannedProcessingInstruction(15))
        );
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn pi_eof() {
        let mut scanner = Scanner::new();
        let bytes = r#"<?test a="b" ?"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningProcessingInstruction)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningProcessingInstruction(true)
        );
        let bytes = r"".as_bytes();
        assert_eq!(scanner.scan(bytes), None);
        assert_eq!(scanner.state, InternalState::Eof);
        assert_eq!(scanner.scan(b">"), None);
        assert_eq!(scanner.state, InternalState::Eof);
    }

    #[test]
    fn pi_with_only_markup_in_first_part() {
        let mut scanner = Scanner::new();
        let bytes = r"<".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
        assert_eq!(scanner.state, InternalState::ScanningMarkup);

        let bytes = r"?test".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningProcessingInstruction)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningProcessingInstruction(false)
        );

        let bytes = r"?>Content".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScannedProcessingInstruction(2))
        );
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn pi_with_question_mark_as_only_part() {
        let mut scanner = Scanner::new();
        let bytes = r"<".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
        assert_eq!(scanner.state, InternalState::ScanningMarkup);

        let bytes = r"?".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningProcessingInstruction)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningProcessingInstruction(false)
        );

        let bytes = r"test ?>Content".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScannedProcessingInstruction(7))
        );
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn pi_in_parts() {
        let mut scanner = Scanner::new();
        let bytes = r"<?test".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningProcessingInstruction)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningProcessingInstruction(false)
        );

        let bytes = r">invalid?>Some content".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScannedProcessingInstruction(10))
        );
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn pi_ensure_does_not_reuse() {
        let mut scanner = Scanner::new();
        let bytes = r"<?".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningProcessingInstruction)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningProcessingInstruction(false)
        );

        let bytes = r">invalid?>Some content".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScannedProcessingInstruction(10))
        );
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn pi_with_broken_delimiter() {
        let mut scanner = Scanner::new();
        let bytes = r"<?test".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningProcessingInstruction)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningProcessingInstruction(false)
        );

        let bytes = r"?".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningProcessingInstruction)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningProcessingInstruction(true)
        );

        let bytes = r#" > a="v""#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningProcessingInstruction)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningProcessingInstruction(false)
        );

        let bytes = r#"?"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningProcessingInstruction)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningProcessingInstruction(true)
        );

        let bytes = r#">"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScannedProcessingInstruction(1))
        );
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn pi_with_single_quotes_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r#"<?goodbye a='val>'?>Content"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScannedProcessingInstruction(20))
        );
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn pi_with_single_quotes_in_parts() {
        let mut scanner = Scanner::new();
        let bytes = r#"<?goodbye a='?"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningProcessingInstruction)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningProcessingInstruction(true)
        );

        let bytes = r#"val?>'?>Content"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScannedProcessingInstruction(5))
        );
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn pi_with_double_quotes_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r#"<?goodbye a="val?>"?>Content"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScannedProcessingInstruction(18))
        );
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn pi_with_double_quotes_in_parts() {
        let mut scanner = Scanner::new();
        let bytes = r#"<?goodbye a="?"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningProcessingInstruction)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningProcessingInstruction(true)
        );

        let bytes = r#"val?>"?>Content"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScannedProcessingInstruction(5))
        );
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn pi_not_reuse_question_mark() {
        let mut scanner = Scanner::new();
        let bytes = r#"<?>"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningProcessingInstruction)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningProcessingInstruction(false)
        );
    }

    #[test]
    fn pi_not_reuse_question_mark_across_parts() {
        let mut scanner = Scanner::new();
        let bytes = r#"<?"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningProcessingInstruction)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningProcessingInstruction(false)
        );

        let bytes = r#">"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningProcessingInstruction)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningProcessingInstruction(false)
        );
    }

    #[test]
    fn declaration_in_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r#"<!DOCTYPE test [<!ELEMENT test (#PCDATA)>]>"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScannedDeclaration(bytes.len()))
        );
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn declaration_eof() {
        let mut scanner = Scanner::new();
        let bytes = r#"<!DOCTYPE test ["#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
        assert_eq!(
            scanner.state,
            InternalState::ScanningDeclaration(
                QuoteState::None,
                BracketCount(1),
                AlreadyFoundByteSeqCount(0)
            )
        );
        let bytes = r"".as_bytes();
        assert_eq!(scanner.scan(bytes), None);
        assert_eq!(scanner.state, InternalState::Eof);
        assert_eq!(scanner.scan(b">"), None);
        assert_eq!(scanner.state, InternalState::Eof);
    }

    #[test]
    fn declaration_with_only_markup_in_first_part() {
        let mut scanner = Scanner::new();
        let bytes = r"<".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
        assert_eq!(scanner.state, InternalState::ScanningMarkup);

        let bytes = r"!ELEMENT".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
        assert_eq!(
            scanner.state,
            InternalState::ScanningDeclaration(
                QuoteState::None,
                BracketCount(0),
                AlreadyFoundByteSeqCount(0)
            )
        );

        let bytes = r">Content".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(1)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn declaration_with_exclamation_as_only_part() {
        let mut scanner = Scanner::new();
        let bytes = r"<".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
        assert_eq!(scanner.state, InternalState::ScanningMarkup);

        let bytes = r"!".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningDeclarationCommentOrCdata)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningDeclarationCommentOrCdata([0; 7], 0)
        );

        let bytes = r"test >Content".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(6)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn declaration_in_parts() {
        let mut scanner = Scanner::new();
        let bytes = r"<!test".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
        assert_eq!(
            scanner.state,
            InternalState::ScanningDeclaration(
                QuoteState::None,
                BracketCount(0),
                AlreadyFoundByteSeqCount(0)
            )
        );

        let bytes = r">Some content".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(1)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn declaration_with_single_quotes_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r#"<!goodbye a='val>'>Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(19)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn declaration_with_single_quotes_in_parts() {
        let mut scanner = Scanner::new();
        let bytes = r#"<!goodbye a='>"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
        assert_eq!(
            scanner.state,
            InternalState::ScanningDeclaration(
                QuoteState::Single,
                BracketCount(0),
                AlreadyFoundByteSeqCount(0)
            )
        );

        let bytes = r#"val>'>Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(6)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn declaration_with_double_quotes_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r#"<!goodbye a="val>">Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(19)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn declaration_with_double_quotes_in_parts() {
        let mut scanner = Scanner::new();
        let bytes = r#"<!goodbye a=">"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
        assert_eq!(
            scanner.state,
            InternalState::ScanningDeclaration(
                QuoteState::Double,
                BracketCount(0),
                AlreadyFoundByteSeqCount(0)
            )
        );

        let bytes = r#"val>">Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(6)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn declaration_with_closed_brackets() {
        let mut scanner = Scanner::new();
        let bytes = r#"<![%test;[<!ELEMENT test (something*)>]]>"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScannedDeclaration(bytes.len()))
        );
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn declaration_with_unclosed_single_bracket() {
        let mut scanner = Scanner::new();
        let bytes = r#"<![test>"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
        assert_eq!(
            scanner.state,
            InternalState::ScanningDeclaration(
                QuoteState::None,
                BracketCount(1),
                AlreadyFoundByteSeqCount(0)
            )
        );

        let bytes = r#">] >Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(4)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn declaration_with_unclosed_double_bracket() {
        let mut scanner = Scanner::new();
        let bytes = r#"<![test>"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
        assert_eq!(
            scanner.state,
            InternalState::ScanningDeclaration(
                QuoteState::None,
                BracketCount(1),
                AlreadyFoundByteSeqCount(0)
            )
        );

        let bytes = r#"[more"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
        assert_eq!(
            scanner.state,
            InternalState::ScanningDeclaration(
                QuoteState::None,
                BracketCount(2),
                AlreadyFoundByteSeqCount(0)
            )
        );

        let bytes = r#">] >Content>"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
        assert_eq!(
            scanner.state,
            InternalState::ScanningDeclaration(
                QuoteState::None,
                BracketCount(1),
                AlreadyFoundByteSeqCount(0)
            )
        );

        let bytes = r#">] >Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(4)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn comment_in_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r#"<!-- Comment -->"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScannedComment(bytes.len()))
        );
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn comment_eof() {
        let mut scanner = Scanner::new();
        let bytes = r#"<!-- Comment"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
        assert_eq!(
            scanner.state,
            InternalState::ScanningComment(AlreadyFoundByteSeqCount(0))
        );
        let bytes = r"".as_bytes();
        assert_eq!(scanner.scan(bytes), None);
        assert_eq!(scanner.state, InternalState::Eof);
        assert_eq!(scanner.scan(b">"), None);
        assert_eq!(scanner.state, InternalState::Eof);
    }

    #[test]
    fn comment_with_only_markup_in_first_part() {
        let mut scanner = Scanner::new();
        let bytes = r"<".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
        assert_eq!(scanner.state, InternalState::ScanningMarkup);

        let bytes = r"!--".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
        assert_eq!(
            scanner.state,
            InternalState::ScanningComment(AlreadyFoundByteSeqCount(0))
        );

        let bytes = r" Comment --> Content".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(12)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn comment_with_exclamation_as_only_part() {
        let mut scanner = Scanner::new();
        let bytes = r"<".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
        assert_eq!(scanner.state, InternalState::ScanningMarkup);

        let bytes = r"!".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningDeclarationCommentOrCdata)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningDeclarationCommentOrCdata([0; 7], 0)
        );

        let bytes = r"-- Comment -->Content".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(14)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn comment_in_parts() {
        let mut scanner = Scanner::new();
        let bytes = r"<!-- test".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
        assert_eq!(
            scanner.state,
            InternalState::ScanningComment(AlreadyFoundByteSeqCount(0))
        );

        let bytes = r" -->Some content".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(4)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn comment_with_single_quotes_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r#"<!-- goodbye a='val-->'-->Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(22)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn comment_with_single_quotes_in_parts() {
        let mut scanner = Scanner::new();
        let bytes = r#"<!--goodbye a='--"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
        assert_eq!(
            scanner.state,
            InternalState::ScanningComment(AlreadyFoundByteSeqCount(2))
        );

        let bytes = r#"val>'-->Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(8)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn comment_with_double_quotes_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r#"<!--goodbye a="val-->"-->Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(21)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn comment_with_double_quotes_in_parts() {
        let mut scanner = Scanner::new();
        let bytes = r#"<!--goodbye a="--"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
        assert_eq!(
            scanner.state,
            InternalState::ScanningComment(AlreadyFoundByteSeqCount(2))
        );

        let bytes = r#"val-->"-->Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(6)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn comment_with_invalid_start() {
        let mut scanner = Scanner::new();
        let bytes = r#"<!-goodbye a="-->"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
        assert_eq!(
            scanner.state,
            InternalState::ScanningDeclaration(
                QuoteState::Double,
                BracketCount(0),
                AlreadyFoundByteSeqCount(0)
            )
        );

        let bytes = r#"val-->">Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(8)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn comment_with_double_dash_inside() {
        let mut scanner = Scanner::new();
        let bytes = r#"<!--goodbye a="--"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
        assert_eq!(
            scanner.state,
            InternalState::ScanningComment(AlreadyFoundByteSeqCount(2))
        );

        let bytes = r#"val-->"-- test -->Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(6)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn comment_with_single_dash() {
        let mut scanner = Scanner::new();
        let bytes = r#"<!--goodbye a="--"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
        assert_eq!(
            scanner.state,
            InternalState::ScanningComment(AlreadyFoundByteSeqCount(2))
        );

        let bytes = r#"val--" test ->Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
        assert_eq!(
            scanner.state,
            InternalState::ScanningComment(AlreadyFoundByteSeqCount(0))
        );

        let bytes = r#"More -->Real Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(8)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn comment_with_split_terminating_delimiter() {
        let mut scanner = Scanner::new();
        let bytes = r#"<!--goodbye a="--"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
        assert_eq!(
            scanner.state,
            InternalState::ScanningComment(AlreadyFoundByteSeqCount(2))
        );

        let bytes = r#"val->" -"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
        assert_eq!(
            scanner.state,
            InternalState::ScanningComment(AlreadyFoundByteSeqCount(1))
        );

        let bytes = r#"-"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
        assert_eq!(
            scanner.state,
            InternalState::ScanningComment(AlreadyFoundByteSeqCount(2))
        );

        let bytes = r#">"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(1)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn comment_not_reused_dashes() {
        let mut scanner = Scanner::new();
        let bytes = r#"<!-->"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
        assert_eq!(
            scanner.state,
            InternalState::ScanningComment(AlreadyFoundByteSeqCount(0))
        );

        let bytes = r#"-->"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedComment(3)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn comment_not_reused_dashes_across_scans() {
        let mut scanner = Scanner::new();
        let bytes = r#"<!--"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
        assert_eq!(
            scanner.state,
            InternalState::ScanningComment(AlreadyFoundByteSeqCount(0))
        );

        let bytes = r#">"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningComment));
        assert_eq!(
            scanner.state,
            InternalState::ScanningComment(AlreadyFoundByteSeqCount(0))
        );
    }

    #[test]
    fn cdata_in_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r#"<![CDATA[ Content ]]>"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(bytes.len())));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn declaration_with_uneven_brackets_in_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r#"<![&random[ Declaration ]]]>"#.as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScannedDeclaration(bytes.len()))
        );
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn cdata_eof() {
        let mut scanner = Scanner::new();
        let bytes = r#"<![CDATA[ Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningCdata));
        assert_eq!(
            scanner.state,
            InternalState::ScanningCdata(AlreadyFoundByteSeqCount(0))
        );
        let bytes = r"".as_bytes();
        assert_eq!(scanner.scan(bytes), None);
        assert_eq!(scanner.state, InternalState::Eof);
        assert_eq!(scanner.scan(b"]]>"), None);
        assert_eq!(scanner.state, InternalState::Eof);
    }

    #[test]
    fn cdata_with_only_markup_in_first_part() {
        let mut scanner = Scanner::new();
        let bytes = r"<".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
        assert_eq!(scanner.state, InternalState::ScanningMarkup);

        let bytes = r"![CDAT".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningDeclarationCommentOrCdata)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningDeclarationCommentOrCdata(
                [b'[', b'C', b'D', b'A', b'T', 0, 0],
                5
            )
        );

        let bytes = r"A[ Content ]]> Unused Content".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(14)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn cdata_with_exclamation_as_only_part() {
        let mut scanner = Scanner::new();
        let bytes = r"<".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningMarkup));
        assert_eq!(scanner.state, InternalState::ScanningMarkup);

        let bytes = r"!".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningDeclarationCommentOrCdata)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningDeclarationCommentOrCdata([0, 0, 0, 0, 0, 0, 0], 0)
        );

        let bytes = r"[CDATA[ Content ]]>Content".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(19)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn cdata_in_parts() {
        let mut scanner = Scanner::new();
        let bytes = r"<![CDA".as_bytes();
        assert_eq!(
            scanner.scan(bytes),
            Some(State::ScanningDeclarationCommentOrCdata)
        );
        assert_eq!(
            scanner.state,
            InternalState::ScanningDeclarationCommentOrCdata([b'[', b'C', b'D', b'A', 0, 0, 0], 4)
        );

        let bytes = r"TA[ Content ]]>Some content".as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(15)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn cdata_with_single_quotes_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r#"<![CDATA[ Content ']]>']]>Unused Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(22)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn cdata_with_single_quotes_in_parts() {
        let mut scanner = Scanner::new();
        let bytes = r#"<![CDATA[ ']>"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningCdata));
        assert_eq!(
            scanner.state,
            InternalState::ScanningCdata(AlreadyFoundByteSeqCount(0))
        );

        let bytes = r#"]>']]>Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(6)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn cdata_with_double_quotes_one_pass() {
        let mut scanner = Scanner::new();
        let bytes = r#"<![CDATA[ goodbye a="]]>"]]>Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(24)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn cdata_with_double_quotes_in_parts() {
        let mut scanner = Scanner::new();
        let bytes = r#"<![CDATA[ a="]>"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningCdata));
        assert_eq!(
            scanner.state,
            InternalState::ScanningCdata(AlreadyFoundByteSeqCount(0))
        );

        let bytes = r#"]>"]]>Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(6)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn cdata_with_invalid_start() {
        let mut scanner = Scanner::new();
        // missing "["
        let bytes = r#"<![CDATA Content a="]]>"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningDeclaration));
        assert_eq!(
            scanner.state,
            InternalState::ScanningDeclaration(
                QuoteState::Double,
                BracketCount(1),
                AlreadyFoundByteSeqCount(0)
            )
        );

        let bytes = r#"]]>"]]>Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedDeclaration(7)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn cdata_with_double_right_bracket_inside() {
        let mut scanner = Scanner::new();
        let bytes = r#"<![CDATA[ Content a="]>"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningCdata));
        assert_eq!(
            scanner.state,
            InternalState::ScanningCdata(AlreadyFoundByteSeqCount(0))
        );

        let bytes = r#"other ]]"]] test ]]>Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(20)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn cdata_with_single_closing_bracket() {
        let mut scanner = Scanner::new();
        let bytes = r#"<![CDATA[ Content a="]>"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningCdata));
        assert_eq!(
            scanner.state,
            InternalState::ScanningCdata(AlreadyFoundByteSeqCount(0))
        );

        let bytes = r#"]>" test ]>Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningCdata));
        assert_eq!(
            scanner.state,
            InternalState::ScanningCdata(AlreadyFoundByteSeqCount(0))
        );

        let bytes = r#"More ]]>Real Content"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(8)));
        assert_eq!(scanner.state, InternalState::Reset);
    }

    #[test]
    fn cdata_with_split_terminating_delimiter() {
        let mut scanner = Scanner::new();
        let bytes = r#"<![CDATA[ goodbye a="]>"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningCdata));
        assert_eq!(
            scanner.state,
            InternalState::ScanningCdata(AlreadyFoundByteSeqCount(0))
        );

        let bytes = r#"val]>" ]"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningCdata));
        assert_eq!(
            scanner.state,
            InternalState::ScanningCdata(AlreadyFoundByteSeqCount(1))
        );

        let bytes = r#"]"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScanningCdata));
        assert_eq!(
            scanner.state,
            InternalState::ScanningCdata(AlreadyFoundByteSeqCount(2))
        );

        let bytes = r#">"#.as_bytes();
        assert_eq!(scanner.scan(bytes), Some(State::ScannedCdata(1)));
        assert_eq!(scanner.state, InternalState::Reset);
    }
}
