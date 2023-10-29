//! Scans byte sequences for tokens.

use crate::bytes::{self, QuoteState};

/// Find the next `>` while being aware of quoted text.
#[inline]
#[must_use]
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
#[must_use]
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
#[must_use]
fn scan_text_content(input: &[u8]) -> &[u8] {
    debug_assert_ne!(0, input.len());
    debug_assert_ne!(input[0], b'<');

    let len = input.iter().position(|b| *b == b'<').unwrap_or(input.len());
    &input[..len]
}

#[inline]
#[must_use]
fn scan_markup(input: &[u8]) -> Option<&[u8]> {
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
#[must_use]
fn scan_start_or_empty_element_tag(input: &[u8]) -> Option<&[u8]> {
    // Skip the head '<'
    const OFFSET: usize = 1;

    // Due to scan_mark(), peek2 is already checked
    debug_assert!(input.len() >= 2);
    debug_assert_eq!(input[0], b'<');
    debug_assert_ne!(input[1], b'/');
    debug_assert_ne!(input[1], b'?');
    debug_assert_ne!(input[1], b'!');

    if let Some(pos) = find_close_tag_char_with_quotes(&input[OFFSET..]) {
        // if pos > 1 && input[OFFSET + pos - 2] == b'/' {
        //     Some(&input[..=pos])
        // } else {
        Some(&input[..=pos])
        // }
    } else {
        None
    }
}

#[inline]
#[must_use]
fn scan_end_tag(input: &[u8]) -> Option<&[u8]> {
    // Skip the head '</'
    const OFFSET: usize = 2;

    debug_assert!(input.len() >= 2);
    debug_assert_eq!(input[0], b'<');
    debug_assert_eq!(input[1], b'/');

    find_close_tag_char_with_quotes(&input[OFFSET..]).map(|pos| &input[..OFFSET + pos])
}

#[inline]
#[must_use]
fn scan_processing_instruction(input: &[u8]) -> Option<&[u8]> {
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
        .map(|pos| &input[..=pos])
}

#[inline]
#[must_use]
fn scan_declaration_comment_or_cdata(input: &[u8]) -> Option<&[u8]> {
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
#[must_use]
fn scan_declaration(input: &[u8]) -> Option<&[u8]> {
    // Skip the head '<!'
    const OFFSET: usize = 2;

    debug_assert!(input.len() >= 2);
    debug_assert_eq!(input[0], b'<');
    debug_assert_eq!(input[1], b'!');

    find_close_tag_char_with_brackets_and_quotes(&input[OFFSET..]).map(|pos| &input[..OFFSET + pos])
}

#[inline]
#[must_use]
fn scan_comment(input: &[u8]) -> Option<&[u8]> {
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
        .map(|pos| &input[..=pos])
}

#[inline]
#[must_use]
fn scan_cdata(input: &[u8]) -> Option<&[u8]> {
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
        .map(|pos| &input[..=pos])
}

/// Scans a slice of bytes from the beginning and attempts to find a token.
///
/// If a token is not found, then there are 2 possible cases:
///
/// 1. The input is malformed.
/// 2. There is more data required to complete the token.
///
/// For instance, the slice of bytes could be `<name`. `None` would be returned
/// from the function. If there is additional input coming, then just append the
/// new data with the existing data and pass all of the data into the function.
///
/// If there is no additional data (e.g. end of file was reached), and there is
/// still data that cannot be scanned as a complete token, then the remaining data
/// usually indicates an error has occurred.
///
/// The function does not carry any state.
/// ```
#[must_use]
pub(super) fn scan(input: &[u8]) -> Option<&[u8]> {
    match bytes::peek(input) {
        None => None,
        Some(b'<') => scan_markup(input),
        Some(_) => Some(scan_text_content(input)),
    }
}
