//! Scans byte sequences for tokens.

use crate::read::parser::{
    self, ScanCdataSectionOpts, ScanCharDataOpts, ScanCommentOpts, ScanMarkupDeclOpts,
    ScanProcessingInstructionOpts, ScanTagOpts,
};

#[inline]
#[must_use]
const fn scan_text_content(input: &[u8], pos: usize) -> Option<usize> {
    debug_assert!(pos < input.len());
    debug_assert!(input[pos] != b'<');

    let end = parser::scan_char_data(
        input,
        pos,
        ScanCharDataOpts {
            allow_ampersand: true,
            allow_cdata_section_close: true,
        },
    );
    if pos == end {
        None
    } else {
        Some(end)
    }
}

#[inline]
#[must_use]
const fn scan_markup(input: &[u8], pos: usize) -> Option<usize> {
    debug_assert!(pos < input.len());
    debug_assert!(input[pos] == b'<');

    let peek2 = pos + 1;
    if input.len() <= peek2 {
        return None;
    }

    match input[peek2] {
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

    // Due to scan_markup(), peek2 is already checked
    debug_assert!(pos + 1 < input.len());
    debug_assert!(input[pos] == b'<');
    debug_assert!(input[pos + 1] != b'/');
    debug_assert!(input[pos + 1] != b'?');
    debug_assert!(input[pos + 1] != b'!');

    parser::scan_s_or_empty_elem_tag_after_prefix(input, pos + OFFSET, ScanTagOpts::new())
}

#[must_use]
const fn scan_end_tag(input: &[u8], pos: usize) -> Option<usize> {
    // Skip the head '</'
    const OFFSET: usize = 2;

    // Due to scan_markup(), peek2 is already checked
    debug_assert!(pos + 1 < input.len());
    debug_assert!(input[pos] == b'<');
    debug_assert!(input[pos + 1] == b'/');

    parser::scan_e_tag_after_prefix(input, pos + OFFSET, ScanTagOpts::new())
}

#[inline]
#[must_use]
const fn scan_processing_instruction(input: &[u8], pos: usize) -> Option<usize> {
    // Skip the head '<?'
    const OFFSET: usize = 2;

    // Due to scan_markup(), peek2 is already checked
    debug_assert!(pos + 1 < input.len());
    debug_assert!(input[pos] == b'<');
    debug_assert!(input[pos + 1] == b'?');

    parser::scan_pi_after_prefix(
        input,
        pos + OFFSET,
        ScanProcessingInstructionOpts {
            assume_valid_xml: false,
            allow_xml_target_name: true,
            allow_all_chars: false,
        },
    )
}

#[inline]
#[must_use]
const fn scan_declaration_comment_or_cdata(input: &[u8], pos: usize) -> Option<usize> {
    // Skip the head '<!'
    const OFFSET: usize = 2;

    // Due to scan_markup(), peek2 is already checked
    debug_assert!(pos + 1 < input.len());
    debug_assert!(input[pos] == b'<');
    debug_assert!(input[pos + 1] == b'!');

    let peek3 = pos + OFFSET;
    if input.len() <= peek3 {
        return None;
    }

    match input[peek3] {
        b'-' => {
            let peek4 = pos + 3;
            if input.len() < peek4 {
                return None;
            }
            match input[peek4] {
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
                scan_cdata(input, pos)
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

    // Due to scan_declaration_comment_or_cdata(), peek3 is already checked
    debug_assert!(pos + 2 < input.len());
    debug_assert!(input[pos] == b'<');
    debug_assert!(input[pos + 1] == b'!');

    parser::scan_doctypedecl_after_prefix(input, pos + OFFSET, ScanMarkupDeclOpts::new())
}

#[inline]
#[must_use]
const fn scan_comment(input: &[u8], pos: usize) -> Option<usize> {
    // Skip the head '<!--'
    const OFFSET: usize = 4;

    // Due to scan_declaration_comment_or_cdata(), peek4 is already checked
    debug_assert!(pos + 3 < input.len());
    debug_assert!(input[pos] == b'<');
    debug_assert!(input[pos + 1] == b'!');
    debug_assert!(input[pos + 2] == b'-');
    debug_assert!(input[pos + 3] == b'-');

    parser::scan_comment_after_prefix(input, pos + OFFSET, ScanCommentOpts::new())
}

#[inline]
#[must_use]
const fn scan_cdata(input: &[u8], pos: usize) -> Option<usize> {
    // Skip the head '<![CDATA['
    const OFFSET: usize = 9;

    // Due to scan_declaration_comment_or_cdata(), peek9 is already checked
    debug_assert!(pos + 8 < input.len());
    debug_assert!(input[pos] == b'<');
    debug_assert!(input[pos + 1] == b'!');
    debug_assert!(input[pos + 2] == b'[');
    debug_assert!(input[pos + 3] == b'C');
    debug_assert!(input[pos + 4] == b'D');
    debug_assert!(input[pos + 5] == b'A');
    debug_assert!(input[pos + 6] == b'T');
    debug_assert!(input[pos + 7] == b'A');
    debug_assert!(input[pos + 8] == b'[');

    parser::scan_cd_sect_after_prefix(input, pos + OFFSET, ScanCdataSectionOpts::new())
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
        _ => scan_text_content(input, pos),
    }
}
