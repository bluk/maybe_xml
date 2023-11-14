//! Scans byte sequences for tokens.

use crate::QuoteState;

/// Find the next `>` while being aware of quoted text.
#[inline]
#[must_use]
const fn find_close_tag_char_with_quotes(input: &[u8], offset: usize) -> Option<usize> {
    let mut quote_state = QuoteState::None;
    let mut index = offset;
    loop {
        if input.len() <= index {
            return None;
        }
        let byte = input[index];
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
        index += 1;
    }
}

/// Find the next `>` while being aware of quoted text and the number of bracket delimiters used.
#[inline]
#[must_use]
const fn find_close_tag_char_with_brackets_and_quotes(
    input: &[u8],
    offset: usize,
) -> Option<usize> {
    let mut bracket_cnt = 0;
    let mut quote_state = QuoteState::None;

    let mut index = offset;
    loop {
        if input.len() <= index {
            return None;
        }

        let byte = input[index];

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
                            return Some(index + 1);
                        }
                        QuoteState::Double | QuoteState::Single => {}
                    }
                }
            }
            _ => {}
        }

        index += 1;
    }
}

#[inline]
#[must_use]
const fn scan_text_content(input: &[u8], pos: usize) -> usize {
    debug_assert!(pos < input.len());
    debug_assert!(input[pos] != b'<');

    let mut index = pos + 1;
    loop {
        if index == input.len() {
            return index;
        }

        if input[index] == b'<' {
            return index;
        }

        index += 1;
    }
}

#[inline]
#[must_use]
const fn scan_markup(input: &[u8], pos: usize) -> Option<usize> {
    debug_assert!(pos < input.len());
    debug_assert!(input[pos] == b'<');

    let peek = pos + 1;
    if input.len() <= peek {
        return None;
    }

    match input[peek] {
        b'/' => scan_end_tag(input, pos),
        b'?' => scan_processing_instruction(input, pos),
        b'!' => scan_declaration_comment_or_cdata(input, pos),
        _ => scan_start_or_empty_element_tag(input, pos),
    }
}

#[inline]
#[must_use]
const fn scan_start_or_empty_element_tag(input: &[u8], pos: usize) -> Option<usize> {
    // Skip the head '<'
    const OFFSET: usize = 1;

    // Due to scan_mark(), peek2 is already checked
    debug_assert!(pos + 1 < input.len());
    debug_assert!(input[pos] == b'<');
    debug_assert!(input[pos + 1] != b'/');
    debug_assert!(input[pos + 1] != b'?');
    debug_assert!(input[pos + 1] != b'!');

    find_close_tag_char_with_quotes(input, pos + OFFSET)
}

#[inline(always)]
#[must_use]
const fn scan_end_tag(input: &[u8], pos: usize) -> Option<usize> {
    // Skip the head '</'
    const OFFSET: usize = 2;

    debug_assert!(pos + 1 < input.len());
    debug_assert!(input[pos] == b'<');
    debug_assert!(input[pos + 1] == b'/');

    find_close_tag_char_with_quotes(input, pos + OFFSET)
}

#[inline]
#[must_use]
const fn scan_processing_instruction(input: &[u8], pos: usize) -> Option<usize> {
    // Skip the head '<?'
    const OFFSET: usize = 2;

    debug_assert!(pos + 1 < input.len());
    debug_assert!(input[pos] == b'<');
    debug_assert!(input[pos + 1] == b'?');

    // Skip OFFSET + 1 because at the minimum, it must be `<??>`.
    // It cannot be `<?>`.

    let mut index = pos + OFFSET + 1;
    loop {
        if input.len() <= index {
            return None;
        }

        let byte = input[index];
        if byte == b'>' && input[index - 1] == b'?' {
            return Some(index + 1);
        }

        index += 1;
    }
}

#[inline]
#[must_use]
const fn scan_declaration_comment_or_cdata(input: &[u8], pos: usize) -> Option<usize> {
    // Skip the head '<!'
    const OFFSET: usize = 2;

    debug_assert!(pos + 1 < input.len());
    debug_assert!(input[pos] == b'<');
    debug_assert!(input[pos + 1] == b'!');

    let peek = pos + 2;
    if input.len() <= peek {
        return None;
    }

    match input[peek] {
        b'-' => {
            let peek2 = pos + 3;
            if input.len() < peek2 {
                return None;
            }
            match input[peek2] {
                b'-' => scan_comment(input, pos),
                _ => scan_declaration(input, pos),
            }
        }
        b'[' => {
            const CDATA: &[u8] = b"[CDATA[";
            if pos + OFFSET + CDATA.len() < input.len()
                && input[pos + 3] == b'C'
                && input[pos + 4] == b'D'
                && input[pos + 5] == b'A'
                && input[pos + 6] == b'T'
                && input[pos + 7] == b'A'
                && input[pos + 8] == b'['
            {
                scan_cdata(input)
            } else {
                scan_declaration(input, pos)
            }
        }
        _ => scan_declaration(input, pos),
    }
}

#[inline]
#[must_use]
const fn scan_declaration(input: &[u8], pos: usize) -> Option<usize> {
    // Skip the head '<!'
    const OFFSET: usize = 2;

    debug_assert!(pos + 1 < input.len());
    debug_assert!(input[pos] == b'<');
    debug_assert!(input[pos + 1] == b'!');

    find_close_tag_char_with_brackets_and_quotes(input, pos + OFFSET)
}

#[inline]
#[must_use]
const fn scan_comment(input: &[u8], pos: usize) -> Option<usize> {
    // Skip the head '<!--'
    const OFFSET: usize = 4;

    debug_assert!(pos + 3 < input.len());
    debug_assert!(input[pos] == b'<');
    debug_assert!(input[pos + 1] == b'!');
    debug_assert!(input[pos + 2] == b'-');
    debug_assert!(input[pos + 3] == b'-');

    // Skip OFFSET + 2 because at the minimum, it must be `<!---->`.
    // It cannot be `<!-->`.

    let mut index = pos + OFFSET + 2;
    loop {
        if input.len() <= index {
            return None;
        }

        let byte = input[index];
        if byte == b'>' && input[index - 1] == b'-' && input[index - 2] == b'-' {
            return Some(index + 1);
        }

        index += 1;
    }
}

#[inline]
#[must_use]
const fn scan_cdata(input: &[u8]) -> Option<usize> {
    // Skip the head '<![CDATA['
    const OFFSET: usize = 9;

    debug_assert!(9 <= input.len());
    debug_assert!(input[0] == b'<');
    debug_assert!(input[1] == b'!');
    debug_assert!(input[2] == b'[');
    debug_assert!(input[3] == b'C');
    debug_assert!(input[4] == b'D');
    debug_assert!(input[5] == b'A');
    debug_assert!(input[6] == b'T');
    debug_assert!(input[7] == b'A');
    debug_assert!(input[8] == b'[');

    // Skip OFFSET + 2 because at the minimum, it must be `<![CDATA[]]>`.

    let mut index = OFFSET + 2;
    loop {
        if input.len() <= index {
            return None;
        }

        let byte = input[index];
        if byte == b'>' && input[index - 1] == b']' && input[index - 2] == b']' {
            return Some(index + 1);
        }

        index += 1;
    }
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
#[inline]
#[must_use]
pub(super) const fn scan(input: &[u8], pos: usize) -> Option<usize> {
    match input[pos] {
        b'<' => scan_markup(input, pos),
        _ => Some(scan_text_content(input, pos)),
    }
}
