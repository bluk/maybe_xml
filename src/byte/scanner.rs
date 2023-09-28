//! Scans byte sequences for tokens.

use crate::{
    bytes::{self, QuoteState},
    token::borrowed::{
        Cdata, Characters, Comment, Declaration, EmptyElementTag, EndTag, ProcessingInstruction,
        StartTag, TokenTy,
    },
};

use super::Token;

/// Find the next `>` while being aware of quoted text.
#[inline]
fn find_close_tag_char_with_quotes(input: &[u8]) -> Option<usize> {
    let mut quote_state = QuoteState::None;
    for (pos, byte) in input.iter().enumerate() {
        match byte {
            b'"' => match quote_state {
                QuoteState::Double => {
                    quote_state = QuoteState::None;
                }
                QuoteState::None => {
                    quote_state = QuoteState::Double;
                }
                QuoteState::Single => {}
            },
            b'\'' => match quote_state {
                QuoteState::Single => {
                    quote_state = QuoteState::None;
                }
                QuoteState::None => {
                    quote_state = QuoteState::Single;
                }
                QuoteState::Double => {}
            },
            b'>' => match quote_state {
                QuoteState::None => {
                    return Some(pos + 1);
                }
                QuoteState::Single | QuoteState::Double => {}
            },
            _ => {}
        }
    }

    None
}

/// Find the next `>` while being aware of quoted text and the number of bracket delimiters used.
#[inline]
fn find_close_tag_char_with_brackets_and_quotes(input: &[u8]) -> Option<usize> {
    let mut bracket_cnt = 0;
    let mut quote_state = QuoteState::None;

    for (pos, byte) in input.iter().enumerate() {
        match byte {
            b'[' => match quote_state {
                QuoteState::None => bracket_cnt += 1,
                QuoteState::Single | QuoteState::Double => {}
            },
            b']' => match quote_state {
                QuoteState::None => {
                    if bracket_cnt > 0 {
                        bracket_cnt -= 1;
                    }
                }
                QuoteState::Single | QuoteState::Double => {}
            },
            b'"' => match quote_state {
                QuoteState::None => {
                    quote_state = QuoteState::Double;
                }
                QuoteState::Double => {
                    quote_state = QuoteState::None;
                }
                QuoteState::Single => {}
            },
            b'\'' => match quote_state {
                QuoteState::None => {
                    quote_state = QuoteState::Single;
                }
                QuoteState::Single => {
                    quote_state = QuoteState::None;
                }
                QuoteState::Double => {}
            },
            b'>' => {
                if bracket_cnt == 0 {
                    match quote_state {
                        QuoteState::None => {
                            return Some(pos + 1);
                        }
                        QuoteState::Double | QuoteState::Single => {}
                    }
                }
            }
            _ => {}
        }
    }

    None
}

#[inline]
fn scan_text_content(input: &[u8]) -> Token<'_> {
    debug_assert_ne!(0, input.len());
    debug_assert_ne!(input[0], b'<');

    let len = input.iter().position(|b| *b == b'<').unwrap_or(input.len());
    Token {
        ty: TokenTy::Characters(Characters::from(&input[..len])),
        offset: 0,
        len,
    }
}

#[inline]
fn scan_markup(input: &[u8]) -> Option<Token<'_>> {
    debug_assert_ne!(0, input.len());
    debug_assert_eq!(input[0], b'<');

    if let Some(next) = bytes::peek2(input) {
        match next {
            b'/' => scan_end_tag(input),
            b'?' => scan_processing_instruction(input),
            b'!' => scan_declaration_comment_or_cdata(input),
            _ => scan_start_or_empty_element_tag(input),
        }
    } else {
        debug_assert_eq!(1, input.len());
        None
    }
}

#[inline]
fn scan_start_or_empty_element_tag(input: &[u8]) -> Option<Token<'_>> {
    // Skip the head '<'
    const OFFSET: usize = 1;

    // Due to scan_mark(), peek2 is already checked
    debug_assert!(input.len() >= 2);
    debug_assert_eq!(input[0], b'<');
    debug_assert_ne!(input[1], b'/');
    debug_assert_ne!(input[1], b'?');
    debug_assert_ne!(input[1], b'!');

    if let Some(pos) = find_close_tag_char_with_quotes(&input[OFFSET..]) {
        if pos > 1 && input[OFFSET + pos - 2] == b'/' {
            Some(Token {
                ty: TokenTy::EmptyElementTag(EmptyElementTag::from(&input[..=pos])),
                offset: 0,
                len: OFFSET + pos,
            })
        } else {
            Some(Token {
                ty: TokenTy::StartTag(StartTag::from(&input[..=pos])),
                offset: 0,
                len: OFFSET + pos,
            })
        }
    } else {
        None
    }
}

#[inline]
fn scan_end_tag(input: &[u8]) -> Option<Token<'_>> {
    // Skip the head '</'
    const OFFSET: usize = 2;

    debug_assert!(input.len() >= 2);
    debug_assert_eq!(input[0], b'<');
    debug_assert_eq!(input[1], b'/');

    find_close_tag_char_with_quotes(&input[OFFSET..]).map(|pos| Token {
        ty: TokenTy::EndTag(EndTag::from(&input[..OFFSET + pos])),
        offset: 0,
        len: OFFSET + pos,
    })
}

#[inline]
fn scan_processing_instruction(input: &[u8]) -> Option<Token<'_>> {
    // Skip the head '<?'
    const OFFSET: usize = 2;

    debug_assert!(input.len() >= 2);
    debug_assert_eq!(input[0], b'<');
    debug_assert_eq!(input[1], b'?');

    // Skip OFFSET + 1 because at the minimum, it must be `<??>`.
    // It cannot be `<?>`.

    input
        .iter()
        .enumerate()
        .skip(OFFSET + 1)
        .find_map(|(pos, b)| {
            if *b == b'>' && input[pos - 1] == b'?' {
                Some(pos)
            } else {
                None
            }
        })
        .map(|pos| Token {
            ty: TokenTy::ProcessingInstruction(ProcessingInstruction::from(&input[..=pos])),
            offset: 0,
            len: pos + 1,
        })
}

#[inline]
fn scan_declaration_comment_or_cdata(input: &[u8]) -> Option<Token<'_>> {
    // Skip the head '<!'
    const OFFSET: usize = 2;

    debug_assert!(input.len() >= 2);
    debug_assert_eq!(input[0], b'<');
    debug_assert_eq!(input[1], b'!');

    let bytes = &input[OFFSET..];
    if let Some(peek) = bytes::peek(bytes) {
        match peek {
            b'-' => {
                if let Some(peek2) = bytes::peek2(bytes) {
                    match peek2 {
                        b'-' => scan_comment(input),
                        _ => scan_declaration(input),
                    }
                } else {
                    None
                }
            }
            b'[' => {
                const CDATA: &[u8] = b"[CDATA[";
                if bytes.len() > CDATA.len() && &bytes[..CDATA.len()] == CDATA {
                    scan_cdata(input)
                } else {
                    scan_declaration(input)
                }
            }
            _ => scan_declaration(input),
        }
    } else {
        None
    }
}

#[inline]
fn scan_declaration(input: &[u8]) -> Option<Token<'_>> {
    // Skip the head '<!'
    const OFFSET: usize = 2;

    debug_assert!(input.len() >= 2);
    debug_assert_eq!(input[0], b'<');
    debug_assert_eq!(input[1], b'!');

    find_close_tag_char_with_brackets_and_quotes(&input[OFFSET..]).map(|pos| Token {
        ty: TokenTy::Declaration(Declaration::from(&input[..OFFSET + pos])),
        offset: 0,
        len: OFFSET + pos,
    })
}

#[inline]
fn scan_comment(input: &[u8]) -> Option<Token<'_>> {
    // Skip the head '<!--'
    const OFFSET: usize = 4;

    debug_assert!(input.len() >= 4);
    debug_assert_eq!(input[0], b'<');
    debug_assert_eq!(input[1], b'!');
    debug_assert_eq!(input[2], b'-');
    debug_assert_eq!(input[3], b'-');

    // Skip OFFSET + 2 because at the minimum, it must be `<!---->`.
    // It cannot be `<!-->`.

    input
        .iter()
        .enumerate()
        .skip(OFFSET + 2)
        .find_map(|(pos, b)| {
            if *b == b'>' && &input[pos - 2..pos] == b"--" {
                Some(pos)
            } else {
                None
            }
        })
        .map(|pos| Token {
            ty: TokenTy::Comment(Comment::from(&input[..=pos])),
            offset: 0,
            len: pos + 1,
        })
}

#[inline]
fn scan_cdata(input: &[u8]) -> Option<Token<'_>> {
    // Skip the head '<![CDATA['
    const OFFSET: usize = 9;

    debug_assert!(input.len() >= 9);
    debug_assert_eq!(input[0], b'<');
    debug_assert_eq!(input[1], b'!');
    debug_assert_eq!(input[2], b'[');
    debug_assert_eq!(input[3], b'C');
    debug_assert_eq!(input[4], b'D');
    debug_assert_eq!(input[5], b'A');
    debug_assert_eq!(input[6], b'T');
    debug_assert_eq!(input[7], b'A');
    debug_assert_eq!(input[8], b'[');

    // Skip OFFSET + 2 because at the minimum, it must be `<![CDATA[]]>`.

    input
        .iter()
        .enumerate()
        .skip(OFFSET + 2)
        .find_map(|(pos, b)| {
            if *b == b'>' && &input[pos - 2..pos] == b"]]" {
                Some(pos)
            } else {
                None
            }
        })
        .map(|pos| Token {
            ty: TokenTy::Cdata(Cdata::from(&input[..=pos])),
            offset: 0,
            len: pos + 1,
        })
}

/// Scans a slice of bytes from the beginning and attempts to find a token.
///
/// Returns `Some(Token)` if a token is found. In this case, the slice of bytes
/// from `&input[Token.offset..Token.len]` can be parsed as a `Token.ty`. After
/// processing the (sub)slice, then you can discard the bytes already processed.
///
/// Returns `None` if a token is not found. If the input is the last bytes to
/// process (e.g.  an end of file was reached), then the remaining input does
/// not form a complete token. Usually, this indicates an error has occurred.
///
/// If there are more bytes to be processed, then append the additional bytes to
/// the existing bytes and pass them new bytes into this function again.
///
/// The function does not carry any state.
///
/// # Examples
///
/// ```
/// use maybe_xml::{
///     byte::{self, Token},
///     token::borrowed::{Characters, StartTag, TokenTy}
/// };
///
/// let mut buffer = Vec::new();
/// buffer.extend(b"<h");
///
/// // The scan cannot find a completed token so `None` is returned.
/// // If there is additional data, append it to the buffer and scan again.
/// assert_eq!(None, byte::scan(&buffer));
///
/// // In an I/O loop (for example), additional data may arrive, so append it to
/// // the existing unprocessed slice of data.
/// buffer.extend(b"ello>Text Content");
///
/// let token = byte::scan(&buffer);
/// assert_eq!(
///     Some(Token {
///         ty: TokenTy::StartTag(StartTag::from("<hello>")),
///         offset: 0,
///         len: 7,
///     }),
///     token
/// );
///
/// let token = token.unwrap();
/// let bytes = &buffer[token.offset..token.offset + token.len];
/// assert_eq!(b"<hello>", bytes);
/// assert_eq!(
///     Ok("hello"),
///     StartTag::from(bytes).name().to_str()
/// );
///
/// // Discard the processed data
/// buffer.drain(token.offset..token.offset + token.len);
///
/// // Scan the remaining data
/// let token = byte::scan(&buffer);
/// assert_eq!(
///     Some(Token {
///         ty: TokenTy::Characters(Characters::from("Text Content")),
///         offset: 0,
///         len: 12,
///     }),
///     token
/// );
///
/// let token = token.unwrap();
/// buffer.drain(token.offset..token.offset + token.len);
/// assert!(buffer.is_empty());
///
/// // If there is no additional data (e.g. end of file is reached), then check
/// // that the buffer is empty. If it is not empty, then there is unprocessed
/// // data which cannot be scanned into a complete token. The left over data
/// // usually indicates an error has occurred.
/// ```
#[must_use]
pub fn scan(input: &[u8]) -> Option<Token<'_>> {
    match bytes::peek(input) {
        None => None,
        Some(b'<') => scan_markup(input),
        Some(_) => Some(scan_text_content(input)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn none_on_empty() {
        assert_eq!(None, scan(b""));
    }

    #[test]
    fn text_content() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::Characters(Characters::new(b"Hello")),
                offset: 0,
                len: 5,
            }),
            scan(b"Hello")
        );
        assert_eq!(
            Some(Token {
                ty: TokenTy::Characters(Characters::new(b" wo")),
                offset: 0,
                len: 3,
            }),
            scan(b" wo")
        );
        assert_eq!(
            Some(Token {
                ty: TokenTy::Characters(Characters::new(b"rld!")),
                offset: 0,
                len: 4,
            }),
            scan(b"rld!<")
        );
    }

    #[test]
    fn incomplete_start_of_markup() {
        assert_eq!(None, scan(b"<"));
    }

    #[test]
    fn start_tag() {
        let bytes = b"<hello>";
        assert_eq!(
            Some(Token {
                ty: TokenTy::StartTag(StartTag::new(bytes)),
                offset: 0,
                len: 7,
            }),
            scan(bytes)
        );
    }

    #[test]
    fn start_tag_with_more_at_end() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::StartTag(StartTag::new(b"<hello>")),
                offset: 0,
                len: 7,
            }),
            scan(b"<hello>Content")
        );
    }

    #[test]
    fn start_tag_with_single_quotes_attribute() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::StartTag(StartTag::new(b"<hello a='val>'>")),
                offset: 0,
                len: 16,
            }),
            scan(b"<hello a='val>'>Content")
        );
    }

    #[test]
    fn start_tag_with_double_quotes_attribute() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::StartTag(StartTag::new(r#"<hello a="val>">"#.as_bytes())),
                offset: 0,
                len: 16,
            }),
            scan(r#"<hello a="val>">Content"#.as_bytes())
        );
    }

    #[test]
    fn empty_element_tag() {
        let bytes = b"<hello/>";
        assert_eq!(
            Some(Token {
                ty: TokenTy::EmptyElementTag(EmptyElementTag::new(bytes)),
                offset: 0,
                len: 8,
            }),
            scan(bytes)
        );
    }

    #[test]
    fn empty_element_tag_space_after_slash_means_start_tag() {
        let bytes = b"<hello / >";
        assert_eq!(
            Some(Token {
                ty: TokenTy::StartTag(StartTag::new(bytes)),
                offset: 0,
                len: 10,
            }),
            scan(bytes)
        );
    }

    #[test]
    fn empty_element_tag_with_double_quotes_attribute() {
        let bytes = r#"<hello a="val/>"/>"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::EmptyElementTag(EmptyElementTag::new(bytes)),
                offset: 0,
                len: 18,
            }),
            scan(bytes)
        );
    }

    #[test]
    fn empty_element_tag_with_last_slash_means_start_tag() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::StartTag(StartTag::new(b"<hello/ invalid>")),
                offset: 0,
                len: 16,
            }),
            scan(b"<hello/ invalid>Content")
        );
    }

    #[test]
    fn end_tag() {
        let bytes = b"</goodbye>";
        assert_eq!(
            Some(Token {
                ty: TokenTy::EndTag(EndTag::new(bytes)),
                offset: 0,
                len: 10,
            }),
            scan(bytes)
        );
    }

    #[test]
    fn end_tag_with_single_quotes_attribute() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::EndTag(EndTag::new(b"</goodbye a='val>'>")),
                offset: 0,
                len: 19,
            }),
            scan(b"</goodbye a='val>'>Content")
        );
    }

    #[test]
    fn end_tag_with_double_quotes_attribute() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::EndTag(EndTag::new(r#"</goodbye a="val>">"#.as_bytes())),
                offset: 0,
                len: 19,
            }),
            scan(r#"</goodbye a="val>">Content"#.as_bytes())
        );
    }

    #[test]
    fn processing_instruction() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::ProcessingInstruction(ProcessingInstruction::new(
                    r#"<?test a="b" ?>"#.as_bytes()
                )),
                offset: 0,
                len: 15,
            }),
            scan(r#"<?test a="b" ?>Some content"#.as_bytes()),
        );
    }

    #[test]
    fn processing_instruction_with_broken_delimiter() {
        let bytes = r#"<?test? > a="v"?>"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::ProcessingInstruction(ProcessingInstruction::new(bytes)),
                offset: 0,
                len: 17,
            }),
            scan(bytes),
        );
    }

    #[test]
    fn pi_with_single_quotes_attribute() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::ProcessingInstruction(ProcessingInstruction::new(
                    r#"<?goodbye a='val>'?>"#.as_bytes()
                )),
                offset: 0,
                len: 20,
            }),
            scan(r#"<?goodbye a='val>'?>Content"#.as_bytes()),
        );
    }

    #[test]
    fn pi_with_double_quotes_attribute() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::ProcessingInstruction(ProcessingInstruction::new(
                    r#"<?goodbye a="val?>"#.as_bytes()
                )),
                offset: 0,
                len: 18,
            }),
            scan(r#"<?goodbye a="val?>"?>Content"#.as_bytes()),
        );
    }

    #[test]
    fn pi_not_reuse_question_mark() {
        // The '>' byte here is treated as part of the tag's content because a "?>" is expected
        let bytes = r#"<?>"#.as_bytes();
        assert_eq!(None, scan(bytes),);
    }

    #[test]
    fn declaration_in_one_pass() {
        let bytes = r#"<!DOCTYPE test [<!ELEMENT test (#PCDATA)>]>"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Declaration(Declaration::new(bytes)),
                offset: 0,
                len: bytes.len(),
            }),
            scan(bytes),
        );
    }

    #[test]
    fn declaration_with_single_quotes_attribute() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::Declaration(Declaration::new(r#"<!goodbye a='val>'>"#.as_bytes())),
                offset: 0,
                len: 19,
            }),
            scan(r#"<!goodbye a='val>'>Content"#.as_bytes()),
        );
    }

    #[test]
    fn declaration_with_double_quotes_attribute() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::Declaration(Declaration::new(r#"<!goodbye a="val>">"#.as_bytes())),
                offset: 0,
                len: 19,
            }),
            scan(r#"<!goodbye a="val>">Content"#.as_bytes()),
        );
    }

    #[test]
    fn declaration_with_closed_brackets() {
        let bytes = r#"<![%test;[<!ELEMENT test (something*)>]]>"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Declaration(Declaration::new(bytes)),
                offset: 0,
                len: bytes.len(),
            }),
            scan(bytes),
        );
    }

    #[test]
    fn declaration_with_unclosed_single_bracket() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::Declaration(Declaration::new(r#"<![test>>] >"#.as_bytes())),
                offset: 0,
                len: 12,
            }),
            scan(r#"<![test>>] >Content"#.as_bytes()),
        );
    }

    #[test]
    fn declaration_with_unclosed_double_bracket() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::Declaration(Declaration::new(
                    r#"<![test>[more>>] >Content>>] >"#.as_bytes()
                )),
                offset: 0,
                len: 30,
            }),
            scan(r#"<![test>[more>>] >Content>>] >Content"#.as_bytes()),
        );
    }

    #[test]
    fn comment_in_one_pass() {
        let bytes = r#"<!-- Comment -->"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Comment(Comment::new(bytes)),
                offset: 0,
                len: bytes.len(),
            }),
            scan(bytes),
        );
    }

    #[test]
    fn comment_with_trailing_data() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::Comment(Comment::new(r#"<!-- Comment -->"#.as_bytes())),
                offset: 0,
                len: 16,
            }),
            scan(r#"<!-- Comment -->Content"#.as_bytes()),
        );
    }

    #[test]
    fn comment_with_single_quotes_attribute() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::Comment(Comment::new(r#"<!-- goodbye a='val-->"#.as_bytes())),
                offset: 0,
                len: 22,
            }),
            scan(r#"<!-- goodbye a='val-->'-->Content"#.as_bytes()),
        );
    }

    #[test]
    fn comment_with_double_quotes_attribute() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::Comment(Comment::new(r#"<!--goodbye a="val-->"#.as_bytes())),
                offset: 0,
                len: 21,
            }),
            scan(r#"<!--goodbye a="val-->"-->Content"#.as_bytes()),
        );
    }

    #[test]
    fn comment_with_invalid_start_means_declaration() {
        let bytes = r#"<!-goodbye a="-->"#.as_bytes();
        assert_eq!(None, scan(bytes));

        assert_eq!(
            Some(Token {
                ty: TokenTy::Declaration(Declaration::new(
                    r#"<!-goodbye a="-->val-->">"#.as_bytes()
                )),
                offset: 0,
                len: 25,
            }),
            scan(r#"<!-goodbye a="-->val-->">Content"#.as_bytes()),
        );
    }

    #[test]
    fn comment_with_double_dash_inside() {
        let bytes = r#"<!--goodbye a="--"#.as_bytes();
        assert_eq!(None, scan(bytes));

        assert_eq!(
            Some(Token {
                ty: TokenTy::Comment(Comment::new(r#"<!--goodbye a="--val-->"#.as_bytes())),
                offset: 0,
                len: 23,
            }),
            scan(r#"<!--goodbye a="--val-->"-- test -->Content"#.as_bytes()),
        );
    }

    #[test]
    fn comment_with_single_dash() {
        assert_eq!(None, scan(r#"<!--goodbye a="--"#.as_bytes()));

        assert_eq!(
            None,
            scan(r#"<!--goodbye a="--val--" test ->Content"#.as_bytes())
        );

        assert_eq!(
            Some(Token {
                ty: TokenTy::Comment(Comment::new(
                    r#"<!--goodbye a="--val--" test ->ContentMore -->"#.as_bytes()
                )),
                offset: 0,
                len: 46,
            }),
            scan(r#"<!--goodbye a="--val--" test ->ContentMore -->Real Content"#.as_bytes()),
        );
    }

    #[test]
    fn comment_not_reused_dashes() {
        let bytes = r#"<!-->-->"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Comment(Comment::new(bytes)),
                offset: 0,
                len: bytes.len(),
            }),
            scan(bytes),
        );
    }

    #[test]
    fn comment_not_reused_dashes_missing_close() {
        let bytes = r#"<!-->"#.as_bytes();
        assert_eq!(None, scan(bytes));
    }

    #[test]
    fn cdata() {
        let bytes = r#"<![CDATA[ Content ]]>"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Cdata(Cdata::new(bytes)),
                offset: 0,
                len: bytes.len(),
            }),
            scan(bytes),
        );
    }

    #[test]
    fn declaration_with_uneven_brackets() {
        let bytes = r#"<![&random[ Declaration ]]]>"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Declaration(Declaration::new(bytes)),
                offset: 0,
                len: bytes.len(),
            }),
            scan(bytes),
        );
    }

    #[test]
    fn cdata_with_trailing_data() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::Cdata(Cdata::new(r"<![CDATA[ Content ]]>".as_bytes())),
                offset: 0,
                len: 21,
            }),
            scan(r"<![CDATA[ Content ]]> Unused Content".as_bytes()),
        );
    }

    #[test]
    fn cdata_with_no_space_trailing_data() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::Cdata(Cdata::new(r"<![CDATA[ Content ]]>".as_bytes())),
                offset: 0,
                len: 21,
            }),
            scan(r"<![CDATA[ Content ]]>Content".as_bytes()),
        );
    }

    #[test]
    fn cdata_with_single_quotes() {
        // The single quote does not escape here
        assert_eq!(
            Some(Token {
                ty: TokenTy::Cdata(Cdata::new(r#"<![CDATA[ Content ']]>"#.as_bytes())),
                offset: 0,
                len: 22,
            }),
            scan(r#"<![CDATA[ Content ']]>']]>Unused Content"#.as_bytes()),
        );
    }

    #[test]
    fn cdata_with_double_quotes() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::Cdata(Cdata::new(r#"<![CDATA[ goodbye a="]]>"#.as_bytes())),
                offset: 0,
                len: 24,
            }),
            scan(r#"<![CDATA[ goodbye a="]]>"]]>Content"#.as_bytes()),
        );
    }

    #[test]
    fn cdata_with_invalid_start_means_declaration() {
        // missing "["
        assert_eq!(None, scan(r#"<![CDATA Content a="]]>"#.as_bytes()));

        assert_eq!(
            Some(Token {
                ty: TokenTy::Declaration(Declaration::new(
                    r#"<![CDATA Content a="]]>]]>"]]>"#.as_bytes()
                )),
                offset: 0,
                len: 30,
            }),
            scan(r#"<![CDATA Content a="]]>]]>"]]>Content"#.as_bytes()),
        );
    }

    #[test]
    fn cdata_with_double_right_bracket_inside() {
        assert_eq!(None, scan(r#"<![CDATA[ Content a="]>"#.as_bytes()));

        assert_eq!(
            Some(Token {
                ty: TokenTy::Cdata(Cdata::new(
                    r#"<![CDATA[ Content a="]>other ]]"]] test ]]>"#.as_bytes()
                )),
                offset: 0,
                len: 43,
            }),
            scan(r#"<![CDATA[ Content a="]>other ]]"]] test ]]>Content"#.as_bytes()),
        );
    }

    #[test]
    fn cdata_with_single_closing_bracket() {
        assert_eq!(None, scan(r#"<![CDATA[ Content a="]>"#.as_bytes()));

        assert_eq!(
            None,
            scan(r#"<![CDATA[ Content a="]>]>" test ]>Content"#.as_bytes())
        );

        assert_eq!(
            Some(Token {
                ty: TokenTy::Cdata(Cdata::new(
                    r#"<![CDATA[ Content a="]>]>" test ]>ContentMore ]]>"#.as_bytes()
                )),
                offset: 0,
                len: 49,
            }),
            scan(r#"<![CDATA[ Content a="]>]>" test ]>ContentMore ]]>Real Content"#.as_bytes()),
        );
    }
}
