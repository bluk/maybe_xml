//! Scans byte sequences for tokens.

use crate::bytes::{self, BracketCount, QuoteState};

use super::{Token, TokenTy};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Offset(usize);

#[inline]
fn quote_context_aware_find(buf: &[u8], mut quote_state: QuoteState) -> Option<usize> {
    for (index, byte) in buf.iter().enumerate() {
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
                    return Some(index + 1);
                }
                QuoteState::Single | QuoteState::Double => {}
            },
            _ => {}
        }
    }

    None
}

#[inline]
fn quote_and_bracket_context_aware_find(
    buf: &[u8],
    quote_state: QuoteState,
    mut bracket_count: BracketCount,
) -> Option<usize> {
    if buf.is_empty() {
        return None;
    }

    let mut read = 0;

    match quote_state {
        QuoteState::None => {
            let prefix_check = b">";
            let buf_len = buf.len();
            let prefix_check_len = prefix_check.len();

            for expected_byte in prefix_check {
                if *expected_byte != buf[read] {
                    break;
                }
                match buf[read] {
                    b'[' => bracket_count.0 += 1,
                    b']' => {
                        if bracket_count.0 > 0 {
                            bracket_count.0 -= 1;
                        }
                    }
                    _ => {}
                };
                read += 1;

                if read == buf_len {
                    if read == prefix_check_len && bracket_count.0 == 0 {
                        return Some(read);
                    }

                    return None;
                }
            }

            if read == prefix_check_len && bracket_count.0 == 0 {
                return Some(read);
            }
        }
        QuoteState::Single | QuoteState::Double => {}
    }

    quote_and_bracket_context_aware_find_2(buf, quote_state, bracket_count, read)
}

#[inline]
fn quote_and_bracket_context_aware_find_2(
    buf: &[u8],
    mut quote_state: QuoteState,
    mut bracket_count: BracketCount,
    mut read: usize,
) -> Option<usize> {
    loop {
        let mut bytes = &buf[read..];
        let mut found_last_byte = false;

        for (index, byte) in bytes.iter().enumerate() {
            match byte {
                b'[' => match quote_state {
                    QuoteState::None => bracket_count.0 += 1,
                    QuoteState::Single | QuoteState::Double => {}
                },
                b']' => match quote_state {
                    QuoteState::None => {
                        if bracket_count.0 > 0 {
                            bracket_count.0 -= 1;
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
                    bytes = &bytes[..=index];
                    found_last_byte = true;
                    break;
                }
                _ => {}
            }
        }
        read += bytes.len();

        if found_last_byte {
            if bracket_count.0 == 0 {
                match quote_state {
                    QuoteState::None => {
                        if read > 0 && buf[read - 1] == b'>' {
                            debug_assert_eq!(find_matching_suffix(b">", &buf[read - 1..read]), 1);
                            return Some(read);
                        }
                    }
                    QuoteState::Double | QuoteState::Single => {}
                }
            }
        } else {
            debug_assert_eq!(read, buf.len());
            return None;
        }
    }
}

#[inline]
fn find_matching_suffix(byte_seq: &[u8], buf: &[u8]) -> usize {
    let buf_len = buf.len();
    let first_byte_seq = byte_seq[0];
    let start_suffix_index = buf_len - usize::min(byte_seq.len(), buf_len);

    let mut byte_seq_index = 0;
    for &b in &buf[start_suffix_index..] {
        if b == byte_seq[byte_seq_index] {
            byte_seq_index += 1;
            continue;
        }

        if b == first_byte_seq {
            byte_seq_index = 1;
        } else {
            byte_seq_index = 0;
        }
    }
    byte_seq_index
}

fn scan_text_content(bytes: &[u8]) -> Token {
    debug_assert_ne!(0, bytes.len());

    Token {
        ty: TokenTy::Characters,
        len: bytes.iter().position(|b| *b == b'<').unwrap_or(bytes.len()),
    }
}

#[inline]
fn scan_markup(bytes: &[u8]) -> Option<Token> {
    let Some(next) = bytes::peek2(bytes) else {
        debug_assert_eq!(bytes.len(), 1);
        return None;
    };

    match next {
        b'/' => scan_end_tag(bytes, QuoteState::None, Offset(2)),
        b'?' => scan_processing_instruction(bytes, false, Offset(2)),
        b'!' => scan_declaration_comment_or_cdata(bytes, [0; 7], 0, Offset(2)),
        _ => scan_start_or_empty_element_tag(bytes, QuoteState::None, false, Offset(1)),
    }
}

fn scan_start_or_empty_element_tag(
    bytes: &[u8],
    quote_state: QuoteState,
    is_last_char_slash: bool,
    offset: Offset,
) -> Option<Token> {
    if bytes.is_empty() {
        return None;
    }

    let Some(read) = quote_context_aware_find(&bytes[(offset.0)..], quote_state) else {
        return None;
    };

    if read > 1 && bytes[offset.0 + read - 2] == b'/' {
        Some(Token {
            ty: TokenTy::EmptyElementTag,
            len: offset.0 + read,
        })
    } else if is_last_char_slash && read == 1 {
        debug_assert_eq!(offset.0, 0);
        Some(Token {
            ty: TokenTy::EmptyElementTag,
            len: read,
        })
    } else {
        Some(Token {
            ty: TokenTy::StartTag,
            len: offset.0 + read,
        })
    }
}

fn scan_end_tag(bytes: &[u8], quote_state: QuoteState, offset: Offset) -> Option<Token> {
    if bytes.is_empty() {
        return None;
    }

    let Some(read) = quote_context_aware_find(&bytes[(offset.0)..], quote_state) else {
        return None;
    };

    Some(Token {
        ty: TokenTy::EndTag,
        len: offset.0 + read,
    })
}

fn scan_processing_instruction(
    bytes: &[u8],
    question_mark_was_seen: bool,
    offset: Offset,
) -> Option<Token> {
    if bytes.is_empty() {
        return None;
    }

    if question_mark_was_seen && bytes.get(offset.0) == Some(&b'>') {
        return Some(Token {
            ty: TokenTy::ProcessingInstruction,
            len: offset.0 + 1,
        });
    }

    let mut bytes_to_search = &bytes[offset.0..];
    let mut read = 0;

    loop {
        if let Some(index) = bytes_to_search.iter().position(|b| *b == b'>') {
            let end = index + 1;
            read += end;

            if index > 0 && bytes_to_search[index - 1] == b'?' {
                return Some(Token {
                    ty: TokenTy::ProcessingInstruction,
                    len: offset.0 + read,
                });
            }

            bytes_to_search = &bytes_to_search[end..];
        } else {
            return None;
        }
    }
}

fn scan_declaration_comment_or_cdata(
    bytes: &[u8],
    mut filled_array: [u8; 7],
    mut filled_count: usize,
    offset: Offset,
) -> Option<Token> {
    if bytes.is_empty() {
        return None;
    }

    let bytes_to_check = &bytes[(offset.0)..];
    let cdata = b"[CDATA[";

    let to_fill = usize::min(filled_array.len() - filled_count, bytes_to_check.len());
    if to_fill > 0 {
        filled_array[filled_count..to_fill + filled_count]
            .copy_from_slice(&bytes_to_check[..to_fill]);
        filled_count += to_fill;
    }

    if filled_count > 0 {
        match filled_array[0] {
            b'-' => {
                if filled_count > 1 {
                    match filled_array[1] {
                        b'-' => scan_comment(bytes, Offset(offset.0 + usize::min(to_fill, 2))),
                        _ => scan_declaration(
                            bytes,
                            QuoteState::None,
                            BracketCount(0),
                            Offset(offset.0),
                        ),
                    }
                } else {
                    debug_assert_eq!(filled_array[0], b'-');
                    debug_assert_eq!(filled_count, 1);
                    None
                }
            }
            b'[' => {
                if filled_array[..filled_count] == cdata[..filled_count] {
                    if filled_count == 7 {
                        scan_cdata(bytes, Offset(offset.0 + to_fill))
                    } else {
                        None
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
                    scan_declaration(
                        bytes,
                        QuoteState::None,
                        BracketCount(bracket_count),
                        Offset(offset.0),
                    )
                }
            }
            _ => scan_declaration(bytes, QuoteState::None, BracketCount(0), Offset(offset.0)),
        }
    } else {
        None
    }
}

fn scan_declaration(
    bytes: &[u8],
    quote_state: QuoteState,
    bracket_count: BracketCount,
    offset: Offset,
) -> Option<Token> {
    if bytes.is_empty() {
        return None;
    }

    let Some(read) =
        quote_and_bracket_context_aware_find(&bytes[(offset.0)..], quote_state, bracket_count)
    else {
        return None;
    };

    Some(Token {
        ty: TokenTy::Declaration,
        len: offset.0 + read,
    })
}

fn scan_comment(bytes: &[u8], offset: Offset) -> Option<Token> {
    if bytes.is_empty() {
        return None;
    }

    let mut bytes_to_search = &bytes[offset.0..];
    let mut read = 0;

    loop {
        if let Some(index) = bytes_to_search.iter().position(|b| *b == b'>') {
            let end = index + 1;
            read += end;

            if index > 1 && &bytes_to_search[index - 2..end] == b"-->" {
                return Some(Token {
                    ty: TokenTy::Comment,
                    len: offset.0 + read,
                });
            }

            bytes_to_search = &bytes_to_search[end..];
        } else {
            return None;
        }
    }
}

fn scan_cdata(bytes: &[u8], offset: Offset) -> Option<Token> {
    if bytes.is_empty() {
        return None;
    }

    let mut bytes_to_search = &bytes[offset.0..];
    let mut read = 0;

    loop {
        if let Some(index) = bytes_to_search.iter().position(|b| *b == b'>') {
            let end = index + 1;
            read += end;

            if index > 1 && &bytes_to_search[index - 2..end] == b"]]>" {
                return Some(Token {
                    ty: TokenTy::Cdata,
                    len: offset.0 + read,
                });
            }

            bytes_to_search = &bytes_to_search[end..];
        } else {
            return None;
        }
    }
}

pub fn scan(bytes: &[u8]) -> Option<Token> {
    match bytes::peek(bytes) {
        None => None,
        Some(b'<') => scan_markup(bytes),
        Some(_) => Some(scan_text_content(bytes)),
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
                ty: TokenTy::Characters,
                len: 5,
            }),
            scan(b"Hello")
        );
        assert_eq!(
            Some(Token {
                ty: TokenTy::Characters,
                len: 3,
            }),
            scan(b" wo")
        );
        assert_eq!(
            Some(Token {
                ty: TokenTy::Characters,
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
        assert_eq!(
            Some(Token {
                ty: TokenTy::StartTag,
                len: 7,
            }),
            scan(b"<hello>")
        );
    }

    #[test]
    fn start_tag_with_more_at_end() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::StartTag,
                len: 7,
            }),
            scan(b"<hello>Content")
        );
    }

    #[test]
    fn start_tag_with_single_quotes_attribute() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::StartTag,
                len: 16,
            }),
            scan(b"<hello a='val>'>Content")
        );
    }

    #[test]
    fn start_tag_with_double_quotes_attribute() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::StartTag,
                len: 16,
            }),
            scan(r#"<hello a="val>">Content"#.as_bytes())
        );
    }

    #[test]
    fn empty_element_tag() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::EmptyElementTag,
                len: 8,
            }),
            scan(b"<hello/>")
        );
    }

    #[test]
    fn empty_element_tag_space_after_slash_means_start_tag() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::StartTag,
                len: 10,
            }),
            scan(b"<hello / >")
        );
    }

    #[test]
    fn empty_element_tag_with_double_quotes_attribute() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::EmptyElementTag,
                len: 18,
            }),
            scan(r#"<hello a="val/>"/>"#.as_bytes())
        );
    }

    #[test]
    fn empty_element_tag_with_last_slash_means_start_tag() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::StartTag,
                len: 16,
            }),
            scan(b"<hello/ invalid>Content")
        );
    }

    #[test]
    fn end_tag() {
        assert_eq!(
            Some(Token {
                ty: TokenTy::EndTag,
                len: 10,
            }),
            scan(b"</goodbye>")
        );
    }

    #[test]
    fn end_tag_with_single_quotes_attribute() {
        let bytes = b"</goodbye a='val>'>Content";
        assert_eq!(
            Some(Token {
                ty: TokenTy::EndTag,
                len: 19,
            }),
            scan(bytes)
        );
        assert_eq!(&bytes[..19], b"</goodbye a='val>'>");
    }

    #[test]
    fn end_tag_with_double_quotes_attribute() {
        let bytes = r#"</goodbye a="val>">Content"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::EndTag,
                len: 19,
            }),
            scan(bytes)
        );
        assert_eq!(&bytes[..19], r#"</goodbye a="val>">"#.as_bytes());
    }

    #[test]
    fn processing_instruction() {
        let bytes = r#"<?test a="b" ?>Some content"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::ProcessingInstruction,
                len: 15,
            }),
            scan(bytes),
        );
    }

    #[test]
    fn processing_instruction_with_broken_delimiter() {
        let bytes = r#"<?test? > a="v"?>"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::ProcessingInstruction,
                len: 17,
            }),
            scan(bytes),
        );
    }

    #[test]
    fn pi_with_single_quotes_attribute() {
        let bytes = r#"<?goodbye a='val>'?>Content"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::ProcessingInstruction,
                len: 20,
            }),
            scan(bytes),
        );
    }

    #[test]
    fn pi_with_double_quotes_attribute() {
        let bytes = r#"<?goodbye a="val?>"?>Content"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::ProcessingInstruction,
                len: 18,
            }),
            scan(bytes),
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
                ty: TokenTy::Declaration,
                len: bytes.len(),
            }),
            scan(bytes),
        );
    }

    #[test]
    fn declaration_with_single_quotes_attribute() {
        let bytes = r#"<!goodbye a='val>'>Content"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Declaration,
                len: 19,
            }),
            scan(bytes),
        );
    }

    #[test]
    fn declaration_with_double_quotes_attribute() {
        let bytes = r#"<!goodbye a="val>">Content"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Declaration,
                len: 19,
            }),
            scan(bytes),
        );
    }

    #[test]
    fn declaration_with_closed_brackets() {
        let bytes = r#"<![%test;[<!ELEMENT test (something*)>]]>"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Declaration,
                len: bytes.len(),
            }),
            scan(bytes),
        );
    }

    #[test]
    fn declaration_with_unclosed_single_bracket() {
        let bytes = r#"<![test>>] >Content"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Declaration,
                len: 12,
            }),
            scan(bytes),
        );
    }

    #[test]
    fn declaration_with_unclosed_double_bracket() {
        let bytes = r#"<![test>[more>>] >Content>>] >Content"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Declaration,
                len: 30,
            }),
            scan(bytes),
        );
    }

    #[test]
    fn comment_in_one_pass() {
        let bytes = r#"<!-- Comment -->"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Comment,
                len: bytes.len(),
            }),
            scan(bytes),
        );
    }

    #[test]
    fn comment_with_trailing_data() {
        let bytes = r#"<!-- Comment -->Content"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Comment,
                len: 16,
            }),
            scan(bytes),
        );
    }

    #[test]
    fn comment_with_single_quotes_attribute() {
        let bytes = r#"<!-- goodbye a='val-->'-->Content"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Comment,
                len: 22,
            }),
            scan(bytes),
        );
    }

    #[test]
    fn comment_with_double_quotes_attribute() {
        let bytes = r#"<!--goodbye a="val-->"-->Content"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Comment,
                len: 21,
            }),
            scan(bytes),
        );
    }

    #[test]
    fn comment_with_invalid_start_means_declaration() {
        let bytes = r#"<!-goodbye a="-->"#.as_bytes();
        assert_eq!(None, scan(bytes));

        let bytes = r#"<!-goodbye a="-->val-->">Content"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Declaration,
                len: 25,
            }),
            scan(bytes),
        );
        assert_eq!(r#"<!-goodbye a="-->val-->">"#.as_bytes(), &bytes[..25]);
    }

    #[test]
    fn comment_with_double_dash_inside() {
        let bytes = r#"<!--goodbye a="--"#.as_bytes();
        assert_eq!(None, scan(bytes));

        let bytes = r#"<!--goodbye a="--val-->"-- test -->Content"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Comment,
                len: 23,
            }),
            scan(bytes),
        );
    }

    #[test]
    fn comment_with_single_dash() {
        let bytes = r#"<!--goodbye a="--"#.as_bytes();
        assert_eq!(None, scan(bytes));

        let bytes = r#"<!--goodbye a="--val--" test ->Content"#.as_bytes();
        assert_eq!(None, scan(bytes));

        let bytes = r#"<!--goodbye a="--val--" test ->ContentMore -->Real Content"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Comment,
                len: 46,
            }),
            scan(bytes),
        );
    }

    #[test]
    fn comment_not_reused_dashes() {
        let bytes = r#"<!-->-->"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Comment,
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
                ty: TokenTy::Cdata,
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
                ty: TokenTy::Declaration,
                len: bytes.len(),
            }),
            scan(bytes),
        );
    }

    #[test]
    fn cdata_with_trailing_data() {
        let bytes = r"<![CDATA[ Content ]]> Unused Content".as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Cdata,
                len: 21,
            }),
            scan(bytes),
        );
    }

    #[test]
    fn cdata_with_no_space_trailing_data() {
        let bytes = r"<![CDATA[ Content ]]>Content".as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Cdata,
                len: 21,
            }),
            scan(bytes),
        );
    }

    #[test]
    fn cdata_with_single_quotes() {
        let bytes = r#"<![CDATA[ Content ']]>']]>Unused Content"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Cdata,
                len: 22,
            }),
            scan(bytes),
        );
        // The single quote does not escape here
        assert_eq!(r#"<![CDATA[ Content ']]>"#.as_bytes(), &bytes[..22]);
    }

    #[test]
    fn cdata_with_double_quotes() {
        let bytes = r#"<![CDATA[ goodbye a="]]>"]]>Content"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Cdata,
                len: 24,
            }),
            scan(bytes),
        );
    }

    #[test]
    fn cdata_with_invalid_start_means_declaration() {
        // missing "["
        let bytes = r#"<![CDATA Content a="]]>"#.as_bytes();
        assert_eq!(None, scan(bytes));

        let bytes = r#"<![CDATA Content a="]]>]]>"]]>Content"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Declaration,
                len: 30,
            }),
            scan(bytes),
        );
        assert_eq!(r#"<![CDATA Content a="]]>]]>"]]>"#.as_bytes(), &bytes[..30]);
    }

    #[test]
    fn cdata_with_double_right_bracket_inside() {
        let bytes = r#"<![CDATA[ Content a="]>"#.as_bytes();
        assert_eq!(None, scan(bytes));

        let bytes = r#"<![CDATA[ Content a="]>other ]]"]] test ]]>Content"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Cdata,
                len: 43,
            }),
            scan(bytes),
        );
        assert_eq!(
            r#"<![CDATA[ Content a="]>other ]]"]] test ]]>"#.as_bytes(),
            &bytes[..43]
        );
    }

    #[test]
    fn cdata_with_single_closing_bracket() {
        let bytes = r#"<![CDATA[ Content a="]>"#.as_bytes();
        assert_eq!(None, scan(bytes));

        let bytes = r#"<![CDATA[ Content a="]>]>" test ]>Content"#.as_bytes();
        assert_eq!(None, scan(bytes));

        let bytes = r#"<![CDATA[ Content a="]>]>" test ]>ContentMore ]]>Real Content"#.as_bytes();
        assert_eq!(
            Some(Token {
                ty: TokenTy::Cdata,
                len: 49,
            }),
            scan(bytes),
        );
        assert_eq!(
            r#"<![CDATA[ Content a="]>]>" test ]>ContentMore ]]>"#.as_bytes(),
            &bytes[..49]
        );
    }
}
