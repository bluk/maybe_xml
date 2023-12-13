const UTF8_CONTINUATION_BYTE_MASK: u8 = 0b0011_1111;

/// Gets the next character from a UTF-8 string.
///
/// The assumption is that the input is a slice of bytes representing a valid
/// UTF-8 string. Due to this assumption, the next character representation must
/// be a complete Unicode scalar value.
#[must_use]
const fn next_ch(input: &[u8], pos: usize) -> Option<(char, usize)> {
    if input.len() <= pos {
        return None;
    }

    let mut index = pos;
    macro_rules! next_byte {
        () => {{
            let byte = input[index];
            index += 1;
            byte
        }};
    }

    let first = next_byte!();

    if first < 0b1000_0000 {
        match char::from_u32(first as u32) {
            Some(ch) => return Some((ch, index)),
            None => return None,
        }
    }

    let second = next_byte!();

    let rest = (second & UTF8_CONTINUATION_BYTE_MASK) as u32;

    if first < 0b1110_0000 {
        let code_pt = (((first & 0b0001_1111) as u32) << 6) | rest;
        match char::from_u32(code_pt) {
            Some(ch) => return Some((ch, index)),
            None => return None,
        }
    }

    let third = next_byte!();

    let rest = rest << 6 | ((third & UTF8_CONTINUATION_BYTE_MASK) as u32);

    if first < 0b1111_0000 {
        let code_pt = (((first & 0b0001_1111) as u32) << 12) | rest;
        match char::from_u32(code_pt) {
            Some(ch) => return Some((ch, index)),
            None => return None,
        }
    }

    let fourth = next_byte!();

    let rest = rest << 6 | ((fourth & UTF8_CONTINUATION_BYTE_MASK) as u32);

    let code_pt = (((first & 0b0000_1111) as u32) << 18) | rest;

    match char::from_u32(code_pt) {
        Some(ch) => Some((ch, index)),
        None => None,
    }
}

/// Peeks if the next character is what is expected and returns the index of the
/// start of the next character.  If it is not the expected character, evaluates
/// to `None`.
macro_rules! peek_ch {
    ($input:expr, $pos:expr, $expected:literal $(,)?) => {
        if let Some((ch, index)) = next_ch($input, $pos) {
            if $expected == ch {
                Some(index)
            } else {
                None
            }
        } else {
            None
        }
    };
    ($input:expr, $pos:expr, $expected:literal, $($expected_rem:literal),+ $(,)?) => {
        if let Some(index) = peek_ch!($input, $pos, $expected) {
            peek_ch!($input, index, $($expected_rem),+)
        } else {
            None
        }
    };
}

/// Expects a character or returns from the function with an expression.
///
/// Like [`peek_ch!`] but returns from the function instead of evaluating the
/// expression to `None`.
macro_rules! expect_ch {
    ($input:expr, $pos:expr $(,)?) => {
        expect_ch!($input, $pos, else None)
    };
    ($input:expr, $pos:expr, else $else_ret_expr:expr $(,)?) => {
        if let Some((ch, idx)) = next_ch($input, $pos) {
            (ch, idx)
        } else {
            return $else_ret_expr;
        }
    };
    ($input:expr, $pos:expr, $expected:literal $(,)?) => {
        expect_ch!($input, $pos, else None, $expected)
    };
    ($input:expr, $pos:expr, else $else_ret_expr:expr, $expected:literal $(,)?) => {
        if let Some((ch, index)) = next_ch($input, $pos) {
            if $expected == ch {
                index
            } else {
                return $else_ret_expr;
            }
        } else {
            return $else_ret_expr;
        }
    };
    ($input:expr, $pos:expr, $expected:literal, $($expected_rem:literal),+ $(,)?) => {
        expect_ch!($input, $pos, else None, $expected, $($expected_rem),+)
    };
    ($input:expr, $pos:expr, else $else_ret_expr:expr, $expected:literal, $($expected_rem:literal),+ $(,)?) => {
        {
            let index = expect_ch!($input, $pos, else $else_ret_expr, $expected);
            expect_ch!($input, index, else $else_ret_expr, $($expected_rem),+)
        }
    };
    ($input:expr, $pos:expr, $expected:expr $(,)?) => {
        expect_ch!($input, $pos, else None, $expected)
    };
    ($input:expr, $pos:expr, else $else_ret_expr:expr, $expected:expr $(,)?) => {
        if let Some((ch, index)) = next_ch($input, $pos) {
            if $expected(ch) {
                index
            } else {
                return $else_ret_expr;
            }
        } else {
            return $else_ret_expr;
        }
    };
}

#[cfg(test)]
#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct ScanDocumentOpts {
    pub(crate) empty_elem: ScanEmptyTagOpts,
    pub(crate) start_tag: ScanStartTagOpts,
    pub(crate) attr_value: ScanAttributeValueOpts,
    pub(crate) cd_sect: ScanCdataSectionOpts,
    pub(crate) char_data: ScanCharDataOpts,
    pub(crate) pi: ScanProcessingInstructionOpts,
    pub(crate) comment: ScanCommentOpts,
}

#[cfg(test)]
#[must_use]
const fn scan_document(input: &[u8], pos: usize, opts: ScanDocumentOpts) -> Option<usize> {
    let Some(idx) = scan_prolog(input, pos, opts) else {
        return None;
    };

    let Some(mut idx) = scan_element(input, idx, opts) else {
        return None;
    };

    while let Some(peek_idx) = scan_misc(
        input,
        idx,
        ScanMiscOpts {
            comment: opts.comment,
            pi: opts.pi,
        },
    ) {
        idx = peek_idx;
    }

    Some(idx)
}

#[inline]
#[must_use]
const fn is_char(ch: char) -> bool {
    matches!(ch,
      '\u{1}'..='\u{D7FF}'
      | '\u{E000}'..='\u{FFFD}'
      | '\u{1_0000}'..='\u{10_FFFF}')
}

#[inline]
#[must_use]
const fn is_space(byte: u8) -> bool {
    // matches!(ch, ' ' | '\t' | '\r' | '\n')
    // matches!(ch, '\u{20}' | '\u{9}' | '\u{D}' | '\u{A}')
    matches!(byte, 32 | 9 | 13 | 10)
}

#[must_use]
const fn scan_space(input: &[u8], pos: usize) -> Option<usize> {
    let mut idx = pos;
    if input.len() <= idx || !is_space(input[idx]) {
        return None;
    }
    idx += 1;

    loop {
        if input.len() <= idx || !is_space(input[idx]) {
            return Some(idx);
        }
        idx += 1;
    }
}

#[inline]
#[must_use]
const fn scan_optional_space(input: &[u8], pos: usize) -> usize {
    if let Some(peek_idx) = scan_space(input, pos) {
        peek_idx
    } else {
        pos
    }
}

#[must_use]
const fn is_name_start_char(ch: char) -> bool {
    matches!(ch, ':'
      | 'A'..='Z'
      | '_'
      | 'a'..='z'
      | '\u{C0}'..='\u{D6}'
      | '\u{D8}'..='\u{F6}'
      | '\u{F8}'..='\u{2FF}'
      | '\u{370}'..='\u{37D}'
      | '\u{375}'..='\u{1FFF}'
      | '\u{200C}'..='\u{200D}'
      | '\u{2070}'..='\u{218F}'
      | '\u{2C00}'..='\u{2FEF}'
      | '\u{3001}'..='\u{D7FF}'
      | '\u{F900}'..='\u{FDCF}'
      | '\u{FDF0}'..='\u{FFFD}'
      | '\u{1_0000}'..='\u{E_FFFF}')
}

#[must_use]
const fn is_name_ch(ch: char) -> bool {
    is_name_start_char(ch)
        || matches!(ch, '-' | '.' | '0'..='9' | '\u{B7}' | '\u{0300}'..='\u{036F}' | '\u{203F}'..='\u{2040}' )
}

#[must_use]
const fn scan_name(input: &[u8], pos: usize) -> Option<usize> {
    let mut idx = expect_ch!(input, pos, is_name_start_char);

    loop {
        idx = expect_ch!(input, idx, else Some(idx), is_name_ch);
    }
}

#[must_use]
const fn scan_nm_token(input: &[u8], pos: usize) -> Option<usize> {
    let mut idx = expect_ch!(input, pos, is_name_ch);

    loop {
        idx = expect_ch!(input, idx, else Some(idx), is_name_ch);
    }
}

#[must_use]
const fn scan_entity_value(input: &[u8], pos: usize) -> Option<usize> {
    let (quote_ch, mut idx) = expect_ch!(input, pos);

    if quote_ch != '"' && quote_ch != '\'' {
        return None;
    }

    loop {
        let (ch, peek_idx) = expect_ch!(input, pos);

        if ch == quote_ch {
            return Some(peek_idx);
        }

        if ch == '%' {
            if let Some(peek_idx) = scan_pe_ref(input, idx) {
                idx = peek_idx;
                continue;
            }

            return None;
        }

        if ch == '&' {
            debug_assert!(peek_idx == idx + 1);
            if let Some(peek_idx) = scan_ref_after_ampersand(input, idx + 1) {
                idx = peek_idx;
                continue;
            }

            return None;
        }

        idx = peek_idx;
    }
}

#[allow(clippy::struct_field_names)]
#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct ScanAttributeValueOpts {
    pub(crate) allow_less_than: bool,
    pub(crate) allow_ampersand: bool,
    pub(crate) allow_no_quote: bool,
}

impl ScanAttributeValueOpts {
    pub(crate) const fn new_compatible() -> Self {
        Self {
            allow_less_than: true,
            allow_ampersand: true,
            allow_no_quote: true,
        }
    }
}

#[must_use]
const fn scan_attribute_value(
    input: &[u8],
    pos: usize,
    opts: ScanAttributeValueOpts,
) -> Option<usize> {
    if input.len() <= pos {
        return None;
    }

    let quote_byte = input[pos];
    let mut idx = pos + 1;

    debug_assert!(!is_space(quote_byte));

    if quote_byte != b'"' && quote_byte != b'\'' {
        if opts.allow_no_quote {
            loop {
                if input.len() <= idx {
                    return None;
                }
                let byte = input[idx];
                let peek_idx = idx + 1;

                if is_space(byte) || byte == b'>' {
                    return Some(idx);
                }

                if byte == b'<' && !opts.allow_less_than {
                    return None;
                }

                if byte == b'&' {
                    if let Some(peek_idx) = scan_ref_after_ampersand(input, idx + 1) {
                        idx = peek_idx;
                        continue;
                    }

                    if !opts.allow_ampersand {
                        return None;
                    }
                }

                idx = peek_idx;
            }
        }

        return None;
    }

    loop {
        if input.len() <= idx {
            return None;
        }
        let byte = input[idx];
        let peek_idx = idx + 1;

        if byte == quote_byte {
            return Some(peek_idx);
        }

        if byte == b'<' && !opts.allow_less_than {
            return None;
        }

        if byte == b'&' {
            if let Some(peek_idx) = scan_ref_after_ampersand(input, idx + 1) {
                idx = peek_idx;
                continue;
            }

            if !opts.allow_ampersand {
                return None;
            }
        }

        idx = peek_idx;
    }
}

#[must_use]
const fn scan_system_literal(input: &[u8], pos: usize) -> Option<usize> {
    let (quote_ch, mut idx) = expect_ch!(input, pos);

    if quote_ch != '"' && quote_ch != '\'' {
        return None;
    }

    loop {
        let (ch, peek_idx) = expect_ch!(input, idx);

        if ch == quote_ch {
            return Some(peek_idx);
        }

        idx = peek_idx;
    }
}

#[must_use]
const fn scan_pub_id_literal(input: &[u8], pos: usize) -> Option<usize> {
    let (quote_ch, mut idx) = expect_ch!(input, pos);

    if quote_ch != '"' && quote_ch != '\'' {
        return None;
    }

    loop {
        let (ch, peek_idx) = expect_ch!(input, idx);

        if ch == quote_ch {
            return Some(peek_idx);
        }

        if !is_pub_id_char(ch) {
            return None;
        }

        idx = peek_idx;
    }
}

#[must_use]
const fn is_pub_id_char(ch: char) -> bool {
    matches!(ch,
    '\u{20}'
    | '\u{D}'
    | '\u{A}'
    | 'a'..='z'
    | 'A'..='Z'
    | '0'..='9'
    | '-'
    | '\''
    | '('
    | ')'
    | '+'
    | ','
    | '.'
    | '/'
    | ':'
    | '='
    | '?'
    | ';'
    | '!'
    | '*'
    | '#'
    | '@'
    | '$'
    | '_'
    | '%'
    )
}

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct ScanCharDataOpts {
    pub(crate) allow_ampersand: bool,
    pub(crate) allow_cdata_section_close: bool,
}

impl ScanCharDataOpts {
    pub(crate) const fn new_compatible() -> Self {
        Self {
            allow_ampersand: true,
            allow_cdata_section_close: true,
        }
    }
}

/// Scans for character data.
#[inline]
#[must_use]
pub(crate) const fn scan_char_data(input: &[u8], pos: usize, opts: ScanCharDataOpts) -> usize {
    let mut idx = pos;
    loop {
        if input.len() == idx {
            return idx;
        }

        let byte = input[idx];

        if byte == b'<' {
            return idx;
        }

        if !opts.allow_ampersand && byte == b'&' {
            return idx;
        }

        if !opts.allow_cdata_section_close
            && byte == b'>'
            && pos <= idx - 2
            && input[idx - 1] == b']'
            && input[idx - 2] == b']'
        {
            return idx - 2;
        }

        idx += 1;
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct ScanCommentOpts {
    pub(crate) allow_double_dash: bool,
    pub(crate) allow_non_chars: bool,
}

impl ScanCommentOpts {
    pub(crate) const fn new_compatible() -> Self {
        Self {
            allow_double_dash: true,
            allow_non_chars: true,
        }
    }
}

/// Scans for a comment.
#[must_use]
const fn scan_comment(input: &[u8], pos: usize, opts: ScanCommentOpts) -> Option<usize> {
    if input.len() <= pos + 3 {
        return None;
    }

    if input[pos] != b'<' {
        return None;
    }
    if input[pos + 1] != b'!' {
        return None;
    }
    if input[pos + 2] != b'-' {
        return None;
    }
    if input[pos + 3] != b'-' {
        return None;
    }

    scan_comment_after_prefix(input, pos + 4, opts)
}

/// Scans for a comment.
#[must_use]
pub(crate) const fn scan_comment_after_prefix(
    input: &[u8],
    pos: usize,
    opts: ScanCommentOpts,
) -> Option<usize> {
    let mut idx = pos;
    if opts.allow_non_chars {
        let mut prev_byte = b' ';

        loop {
            loop {
                if input.len() <= idx {
                    return None;
                }
                let byte = input[idx];

                let is_double_dash = byte == b'-' && prev_byte == b'-';
                idx += 1;
                prev_byte = byte;

                if is_double_dash {
                    break;
                }
            }

            if input.len() <= idx {
                return None;
            }
            if input[idx] == b'>' {
                return Some(idx + 1);
            } else if !opts.allow_double_dash {
                return None;
            }
        }
    } else {
        let mut prev_ch = ' ';

        loop {
            loop {
                let (ch, peek_idx) = expect_ch!(input, idx);

                if !is_char(ch) {
                    return None;
                }

                let is_double_dash = ch == '-' && prev_ch == '-';
                idx = peek_idx;
                prev_ch = ch;

                if is_double_dash {
                    break;
                }
            }

            if let Some(peek_idx) = peek_ch!(input, idx, '>') {
                return Some(peek_idx);
            } else if !opts.allow_double_dash {
                return None;
            }
        }
    }
}

#[allow(clippy::struct_field_names)]
#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct ScanProcessingInstructionOpts {
    pub(crate) allow_xml_target_name: bool,
    pub(crate) allow_all_chars: bool,
    pub(crate) allow_non_space_after_pi_target: bool,
}

impl ScanProcessingInstructionOpts {
    pub(crate) const fn new_compatible() -> Self {
        Self {
            allow_xml_target_name: true,
            allow_all_chars: true,
            allow_non_space_after_pi_target: true,
        }
    }
}

/// Scans for Processing Instructions
#[must_use]
pub(crate) const fn scan_pi(
    input: &[u8],
    pos: usize,
    opts: ScanProcessingInstructionOpts,
) -> Option<usize> {
    // TODO: Can optimize because the leading characters may have been peeked at
    let idx = expect_ch!(input, pos, '<', '?');

    let start_pi_target = idx;
    let Some(mut idx) = scan_name(input, idx) else {
        return None;
    };

    if !opts.allow_xml_target_name
        && idx - start_pi_target == 3
        && (input[start_pi_target] == b'x' || input[start_pi_target] == b'X')
        && (input[start_pi_target + 1] == b'm' || input[start_pi_target + 1] == b'M')
        && (input[start_pi_target + 2] == b'l' || input[start_pi_target + 2] == b'L')
    {
        // TODO: Would return error here on parsing errors
        return None;
    }

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    } else {
        if let Some(peek_idx) = peek_ch!(input, idx, '?') {
            if let Some(peek_idx) = peek_ch!(input, peek_idx, '>') {
                return Some(peek_idx);
            };
        }

        if !opts.allow_non_space_after_pi_target {
            return None;
        }
    }

    loop {
        let (ch, peek_idx) = expect_ch!(input, idx);

        if !opts.allow_all_chars && !is_char(ch) {
            return None;
        }

        if ch == '>' && pos < idx && input[idx - 1] == b'?' {
            return Some(peek_idx);
        }

        idx = peek_idx;
    }
}

#[allow(clippy::struct_field_names)]
#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct ScanCdataSectionOpts {
    pub(crate) allow_all_chars: bool,
}

impl ScanCdataSectionOpts {
    pub(crate) const fn new_compatible() -> Self {
        Self {
            allow_all_chars: true,
        }
    }
}

/// Scans for CDATA section
#[must_use]
pub(crate) const fn scan_cd_sect(
    input: &[u8],
    pos: usize,
    opts: ScanCdataSectionOpts,
) -> Option<usize> {
    // TODO: Can optimize because the leading characters may have been peeked at

    let mut idx = expect_ch!(input, pos, '<', '!', '[', 'C', 'D', 'A', 'T', 'A', '[');

    loop {
        let (ch, peek_idx) = expect_ch!(input, idx);

        if !opts.allow_all_chars && !is_char(ch) {
            return None;
        }

        if ch == '>' && pos <= idx - 2 && input[idx - 1] == b']' && input[idx - 2] == b']' {
            return Some(peek_idx);
        }

        idx = peek_idx;
    }
}

#[cfg(test)]
#[must_use]
const fn scan_prolog(input: &[u8], pos: usize, opts: ScanDocumentOpts) -> Option<usize> {
    let Some(mut idx) = scan_xml_decl(input, pos) else {
        return None;
    };

    loop {
        let Some(peek_idx) = scan_misc(
            input,
            idx,
            ScanMiscOpts {
                comment: opts.comment,
                pi: opts.pi,
            },
        ) else {
            break;
        };
        idx = peek_idx;
    }

    if let Some(peek_idx) = scan_doctype_decl(
        input,
        idx,
        ScanMarkupDeclOpts {
            comment: opts.comment,
            pi: opts.pi,
            attr_value: opts.attr_value,
        },
    ) {
        idx = peek_idx;

        loop {
            let Some(peek_idx) = scan_misc(
                input,
                idx,
                ScanMiscOpts {
                    comment: opts.comment,
                    pi: opts.pi,
                },
            ) else {
                break;
            };
            idx = peek_idx;
        }
    }

    Some(idx)
}

#[cfg(test)]
#[must_use]
const fn scan_xml_decl(input: &[u8], pos: usize) -> Option<usize> {
    let idx = expect_ch!(input, pos, '<', '?', 'x', 'm', 'l');

    let Some(mut idx) = scan_version_info(input, idx) else {
        return None;
    };

    if let Some(peek_idx) = scan_encoding_decl(input, idx) {
        idx = peek_idx;
    }

    if let Some(peek_idx) = scan_sd_decl(input, idx) {
        idx = peek_idx;
    }

    let idx = scan_optional_space(input, idx);

    Some(expect_ch!(input, idx, '?', '>'))
}

#[cfg(test)]
#[must_use]
const fn scan_version_info(input: &[u8], pos: usize) -> Option<usize> {
    let Some(idx) = scan_space(input, pos) else {
        return None;
    };

    let idx = expect_ch!(input, idx, 'v', 'e', 'r', 's', 'i', 'o', 'n');

    let Some(idx) = scan_eq(input, idx) else {
        return None;
    };

    let (quote_ch, mut idx) = expect_ch!(input, idx);

    if quote_ch != '"' && quote_ch != '\'' {
        // TODO: Add no quotes option?
        return None;
    }

    loop {
        let (ch, peek_idx) = expect_ch!(input, idx);

        if ch == quote_ch {
            return Some(peek_idx);
        }

        idx = peek_idx;
    }
}

#[must_use]
const fn scan_eq(input: &[u8], pos: usize) -> Option<usize> {
    let idx = scan_optional_space(input, pos);
    let idx = expect_ch!(input, idx, '=');
    let idx = scan_optional_space(input, idx);
    Some(idx)
}

#[cfg(test)]
#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct ScanMiscOpts {
    pub(crate) comment: ScanCommentOpts,
    pub(crate) pi: ScanProcessingInstructionOpts,
}

#[cfg(test)]
#[must_use]
const fn scan_misc(input: &[u8], pos: usize, opts: ScanMiscOpts) -> Option<usize> {
    // TODO: Should peek at the first character and decide what to do

    if let Some(idx) = scan_comment(input, pos, opts.comment) {
        return Some(idx);
    }

    if let Some(idx) = scan_pi(input, pos, opts.pi) {
        return Some(idx);
    }

    scan_space(input, pos)
}

#[must_use]
pub(crate) const fn scan_doctype_decl(
    input: &[u8],
    pos: usize,
    opts: ScanMarkupDeclOpts,
) -> Option<usize> {
    // TODO: Can optimize because the leading characters may have been peeked at

    let idx = expect_ch!(input, pos, '<', '!', 'D', 'O', 'C', 'T', 'Y', 'P', 'E');

    let Some(idx) = scan_space(input, idx) else {
        return None;
    };

    let Some(mut idx) = scan_name(input, idx) else {
        return None;
    };

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;

        if let Some(peek_idx) = scan_external_id(input, idx) {
            idx = scan_optional_space(input, peek_idx);
        }
    }

    if let Some(peek_idx) = peek_ch!(input, idx, '[') {
        idx = scan_int_subset(input, peek_idx, opts);
        let peek_idx = expect_ch!(input, idx, ']');
        idx = scan_optional_space(input, peek_idx);
    }

    Some(expect_ch!(input, idx, '>'))
}

#[must_use]
const fn scan_decl_sep(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Should peek at the first character and decide what to do

    if let Some(idx) = scan_pe_ref(input, pos) {
        return Some(idx);
    }

    scan_space(input, pos)
}

#[must_use]
const fn scan_int_subset(input: &[u8], pos: usize, opts: ScanMarkupDeclOpts) -> usize {
    // TODO: Should peek at the first character and decide what to do
    let mut idx = pos;

    loop {
        if let Some(peek_idx) = scan_markup_decl(input, idx, opts) {
            idx = peek_idx;
        } else if let Some(peek_idx) = scan_decl_sep(input, idx) {
            idx = peek_idx;
        } else {
            break;
        }
    }

    idx
}

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct ScanMarkupDeclOpts {
    pub(crate) comment: ScanCommentOpts,
    pub(crate) pi: ScanProcessingInstructionOpts,
    pub(crate) attr_value: ScanAttributeValueOpts,
}

impl ScanMarkupDeclOpts {
    pub(crate) const fn new_compatible() -> Self {
        Self {
            pi: ScanProcessingInstructionOpts::new_compatible(),
            comment: ScanCommentOpts::new_compatible(),
            attr_value: ScanAttributeValueOpts::new_compatible(),
        }
    }
}

#[must_use]
const fn scan_markup_decl(input: &[u8], pos: usize, opts: ScanMarkupDeclOpts) -> Option<usize> {
    // TODO: Should peek at the first character and decide what to do
    if let Some(idx) = scan_element_decl(input, pos) {
        return Some(idx);
    }

    if let Some(idx) = scan_att_list_decl(input, pos, opts.attr_value) {
        return Some(idx);
    }

    if let Some(idx) = scan_entity_decl(input, pos) {
        return Some(idx);
    }

    if let Some(idx) = scan_notiation_decl(input, pos) {
        return Some(idx);
    }

    if let Some(idx) = scan_pi(input, pos, opts.pi) {
        return Some(idx);
    }

    if let Some(idx) = scan_comment(input, pos, opts.comment) {
        return Some(idx);
    }

    None
}

// XXX: Missing 30, 31

#[cfg(test)]
#[must_use]
const fn scan_sd_decl(input: &[u8], pos: usize) -> Option<usize> {
    let Some(idx) = scan_space(input, pos) else {
        return None;
    };

    let idx = expect_ch!(input, idx, 's', 't', 'a', 'n', 'd', 'a', 'l', 'o', 'n', 'e');
    let Some(idx) = scan_eq(input, idx) else {
        return None;
    };
    let (quote_ch, mut idx) = expect_ch!(input, idx);

    if quote_ch != '"' && quote_ch != '\'' {
        // TODO: Add no quotes option?
        return None;
    }

    loop {
        let (ch, peek_idx) = expect_ch!(input, idx);

        // TODO: Be stricter and only allow ('yes' | 'no')?

        if ch == quote_ch {
            return Some(peek_idx);
        }

        idx = peek_idx;
    }
}

#[cfg(test)]
#[must_use]
const fn scan_element(input: &[u8], pos: usize, opts: ScanDocumentOpts) -> Option<usize> {
    if let Some(peek_idx) = scan_empty_tag(input, pos, opts.empty_elem) {
        return Some(peek_idx);
    }

    // Or...

    let Some(mut idx) = scan_start_tag(input, pos, opts.start_tag) else {
        return None;
    };

    let Some(start_name_end) = scan_name(input, pos + 1) else {
        unreachable!();
    };
    let (start_name, _) = input.split_at(start_name_end);
    let (_, start_name) = start_name.split_at(pos + 1);

    idx = scan_content(input, idx, opts);

    let end_tag_start = idx;

    let Some(idx) = scan_end_tag(input, idx, false) else {
        return None;
    };

    let Some(end_name_end) = scan_name(input, end_tag_start + 2) else {
        unreachable!();
    };
    let (end_name, _) = input.split_at(end_name_end);
    let (_, end_name) = end_name.split_at(end_tag_start + 2);

    // Verifying the start tag name is equal to the end tag name byte for byte

    if start_name.len() != end_name.len() {
        return None;
    }

    let mut name_idx = 0;
    loop {
        if name_idx == start_name.len() {
            break;
        }

        if start_name[name_idx] != end_name[name_idx] {
            return None;
        }

        name_idx += 1;
    }

    Some(idx)
}

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct ScanStartTagOpts {
    pub(crate) allow_slash: bool,
    pub(crate) attr_opts: ScanAttributeOpts,
}

impl ScanStartTagOpts {
    pub(crate) const fn new_compatible() -> Self {
        Self {
            allow_slash: true,
            attr_opts: ScanAttributeOpts::new_compatible(),
        }
    }
}

// XXX: Skip 39

#[allow(clippy::fn_params_excessive_bools)]
#[must_use]
pub(crate) const fn scan_start_tag(
    input: &[u8],
    pos: usize,
    opts: ScanStartTagOpts,
) -> Option<usize> {
    // TODO: Can optimize because the leading character may have been peeked at
    let idx = expect_ch!(input, pos, '<');

    let Some(mut idx) = scan_name(input, idx) else {
        return None;
    };

    loop {
        let mut peek_idx = idx;
        if let Some(peek_space_idx) = scan_space(input, peek_idx) {
            if let Some(peek_idx) = scan_attribute(input, peek_space_idx, opts.attr_opts) {
                idx = peek_idx;
                continue;
            }
            peek_idx = peek_space_idx;
        }

        if opts.allow_slash {
            // TODO: In normal parsing, would just emit error here
            if let Some(peek_idx) = peek_ch!(input, peek_idx, '/') {
                if peek_ch!(input, peek_idx, '>').is_some() {
                    return None;
                }

                idx = peek_idx;
                continue;
            }
        }

        break;
    }

    let idx = scan_optional_space(input, idx);

    Some(expect_ch!(input, idx, '>'))
}

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct ScanAttributeOpts {
    pub(crate) allow_no_value: bool,
    pub(crate) attr_value_opts: ScanAttributeValueOpts,
}

impl ScanAttributeOpts {
    pub(crate) const fn new_compatible() -> Self {
        Self {
            allow_no_value: true,
            attr_value_opts: ScanAttributeValueOpts::new_compatible(),
        }
    }
}

#[must_use]
const fn scan_attribute(input: &[u8], pos: usize, opts: ScanAttributeOpts) -> Option<usize> {
    let Some(idx) = scan_name(input, pos) else {
        return None;
    };

    let Some(idx) = scan_eq(input, idx) else {
        if opts.allow_no_value {
            return Some(idx);
        }

        return None;
    };

    scan_attribute_value(input, idx, opts.attr_value_opts)
}

#[cfg(test)]
#[inline]
#[must_use]
const fn scan_end_tag(
    input: &[u8],
    pos: usize,
    allow_more_than_name_and_trailing_space: bool,
) -> Option<usize> {
    if input.len() <= pos + 1 {
        return None;
    }
    if input[pos] != b'<' {
        return None;
    }
    if input[pos + 1] != b'/' {
        return None;
    }

    scan_end_tag_after_prefix(input, pos + 2, allow_more_than_name_and_trailing_space)
}

#[inline]
#[must_use]
pub(crate) const fn scan_end_tag_after_prefix(
    input: &[u8],
    pos: usize,
    allow_more_than_name_and_trailing_space: bool,
) -> Option<usize> {
    if allow_more_than_name_and_trailing_space {
        debug_assert!(!is_name_ch('>'));

        let mut idx = pos;
        loop {
            if input.len() == idx {
                return None;
            }

            if input[idx] == b'>' {
                return Some(idx + 1);
            }

            idx += 1;
        }
    }

    let Some(idx) = scan_name(input, pos) else {
        return None;
    };

    let idx = scan_optional_space(input, idx);

    if input.len() == idx {
        return None;
    }

    if input[idx] != b'>' {
        return None;
    }

    Some(idx + 1)
}

#[cfg(test)]
#[must_use]
const fn scan_content(input: &[u8], pos: usize, opts: ScanDocumentOpts) -> usize {
    let mut idx = scan_char_data(input, pos, opts.char_data);

    loop {
        if let Some(peek_idx) = scan_element(input, idx, opts) {
            idx = peek_idx;
        } else if let Some(peek_idx) = scan_ref(input, idx) {
            idx = peek_idx;
        } else if let Some(peek_idx) = scan_cd_sect(input, idx, opts.cd_sect) {
            idx = peek_idx;
        } else if let Some(peek_idx) = scan_pi(input, idx, opts.pi) {
            idx = peek_idx;
        } else if let Some(peek_idx) = scan_comment(input, idx, opts.comment) {
            idx = peek_idx;
        } else {
            break;
        }

        idx = scan_char_data(input, idx, opts.char_data);
    }

    idx
}

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct ScanEmptyTagOpts {
    pub(crate) allow_slash: bool,
    pub(crate) attr_opts: ScanAttributeOpts,
}

impl ScanEmptyTagOpts {
    pub(crate) const fn new_compatible() -> Self {
        Self {
            allow_slash: true,
            attr_opts: ScanAttributeOpts::new_compatible(),
        }
    }
}

#[must_use]
pub(crate) const fn scan_empty_tag(
    input: &[u8],
    pos: usize,
    opts: ScanEmptyTagOpts,
) -> Option<usize> {
    // TODO: Can optimize because the leading character may have been peeked at

    let idx = expect_ch!(input, pos, '<');
    let Some(mut idx) = scan_name(input, idx) else {
        return None;
    };

    loop {
        let mut peek_idx = idx;

        if let Some(peek_space_idx) = scan_space(input, peek_idx) {
            if let Some(peek_idx) = scan_attribute(input, peek_space_idx, opts.attr_opts) {
                idx = peek_idx;
                continue;
            }
            peek_idx = peek_space_idx;
        }

        if opts.allow_slash {
            // TODO: In normal parsing, would just emit error here
            if let Some(peek_idx) = peek_ch!(input, peek_idx, '/') {
                if peek_ch!(input, peek_idx, '>').is_some() {
                    break;
                }

                idx = peek_idx;
                continue;
            }
        }

        break;
    }

    let idx = scan_optional_space(input, idx);

    Some(expect_ch!(input, idx, '/', '>'))
}

#[must_use]
const fn scan_element_decl(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Can optimize because the leading characters may have been peeked at
    let idx = expect_ch!(input, pos, '<', '!', 'E', 'L', 'E', 'M', 'E', 'N', 'T');
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };
    let Some(idx) = scan_name(input, idx) else {
        return None;
    };
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };

    let Some(idx) = scan_content_spec(input, idx) else {
        return None;
    };

    let idx = scan_optional_space(input, idx);

    Some(expect_ch!(input, idx, '>'))
}

#[must_use]
const fn scan_content_spec(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Should peek to decide which branch

    if let Some(idx) = peek_ch!(input, pos, 'E', 'M', 'P', 'T', 'Y') {
        return Some(idx);
    };

    if let Some(idx) = peek_ch!(input, pos, 'A', 'N', 'Y') {
        return Some(idx);
    };

    if let Some(idx) = scan_mixed(input, pos) {
        return Some(idx);
    };

    if let Some(idx) = scan_children(input, pos) {
        return Some(idx);
    };

    None
}

#[must_use]
const fn scan_children(input: &[u8], pos: usize) -> Option<usize> {
    let mut idx = pos;
    if let Some(peek_idx) = scan_choice(input, idx) {
        idx = peek_idx;
    } else if let Some(peek_idx) = scan_seq(input, idx) {
        idx = peek_idx;
    } else {
        return None;
    }

    if let Some((ch, peek_idx)) = next_ch(input, idx) {
        match ch {
            '?' | '*' | '+' => return Some(peek_idx),
            _ => return Some(idx),
        }
    }

    Some(idx)
}

#[must_use]
const fn scan_cp(input: &[u8], pos: usize) -> Option<usize> {
    let mut idx = pos;
    if let Some(peek_idx) = scan_name(input, idx) {
        idx = peek_idx;
    } else if let Some(peek_idx) = scan_choice(input, idx) {
        idx = peek_idx;
    } else if let Some(peek_idx) = scan_seq(input, idx) {
        idx = peek_idx;
    } else {
        return None;
    }

    if let Some((ch, peek_idx)) = next_ch(input, idx) {
        match ch {
            '?' | '*' | '+' => return Some(peek_idx),
            _ => return Some(idx),
        }
    }

    Some(idx)
}

#[must_use]
const fn scan_choice(input: &[u8], pos: usize) -> Option<usize> {
    let idx = expect_ch!(input, pos, '(');

    let idx = scan_optional_space(input, idx);

    let Some(idx) = scan_cp(input, idx) else {
        return None;
    };

    let idx = scan_optional_space(input, idx);

    let idx = expect_ch!(input, idx, '|');

    let idx = scan_optional_space(input, idx);

    let Some(mut idx) = scan_cp(input, idx) else {
        return None;
    };

    loop {
        let choices_idx = scan_optional_space(input, idx);

        let Some(choices_idx) = peek_ch!(input, choices_idx, '|') else {
            break;
        };

        let choices_idx = scan_optional_space(input, choices_idx);

        let Some(peek_idx) = scan_cp(input, choices_idx) else {
            return None;
        };
        idx = peek_idx;
    }

    let idx = scan_optional_space(input, idx);

    Some(expect_ch!(input, idx, ')'))
}

#[must_use]
const fn scan_seq(input: &[u8], pos: usize) -> Option<usize> {
    let idx = expect_ch!(input, pos, '(');

    let idx = scan_optional_space(input, idx);

    let Some(mut idx) = scan_cp(input, idx) else {
        return None;
    };

    loop {
        let seq_idx = scan_optional_space(input, idx);

        let Some(seq_idx) = peek_ch!(input, seq_idx, ',') else {
            break;
        };

        let seq_idx = scan_optional_space(input, seq_idx);

        let Some(peek_idx) = scan_cp(input, seq_idx) else {
            return None;
        };
        idx = peek_idx;
    }

    let idx = scan_optional_space(input, idx);

    Some(expect_ch!(input, idx, ')'))
}

#[must_use]
const fn scan_mixed(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Could optimize if the leading character is peaked?

    let idx = expect_ch!(input, pos, '(');

    let idx = scan_optional_space(input, idx);

    let idx = expect_ch!(input, idx, '#', 'P', 'C', 'D', 'A', 'T', 'A');

    let idx = scan_optional_space(input, idx);

    // Check for an early exit

    let (ch, idx) = expect_ch!(input, idx);
    if ch == ')' {
        let (ch, peek_idx) = expect_ch!(input, idx, else Some(idx));

        if ch == '*' {
            return Some(peek_idx);
        }

        return Some(idx);
    }

    if ch != '|' {
        return None;
    }

    let idx = scan_optional_space(input, idx);

    let Some(mut idx) = scan_name(input, idx) else {
        return None;
    };

    loop {
        idx = scan_optional_space(input, idx);

        let (ch, peek_idx) = expect_ch!(input, idx);
        if ch == ')' {
            let (ch, peek_2_idx) = expect_ch!(input, peek_idx);

            if ch != '*' {
                return None;
            }

            return Some(peek_2_idx);
        }

        if ch != '|' {
            return None;
        }
        idx = peek_idx;

        idx = scan_optional_space(input, idx);

        let Some(peek_idx) = scan_name(input, idx) else {
            return None;
        };
        idx = peek_idx;
    }
}

#[must_use]
const fn scan_att_list_decl(
    input: &[u8],
    pos: usize,
    opts: ScanAttributeValueOpts,
) -> Option<usize> {
    let idx = expect_ch!(input, pos, '<', '!', 'A', 'T', 'T', 'L', 'I', 'S', 'T');
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };
    let Some(mut idx) = scan_name(input, idx) else {
        return None;
    };

    while let Some(peek_idx) = scan_att_def(input, idx, opts) {
        idx = peek_idx;
    }

    let idx = scan_optional_space(input, idx);

    Some(expect_ch!(input, idx, '>'))
}

#[must_use]
const fn scan_att_def(input: &[u8], pos: usize, opts: ScanAttributeValueOpts) -> Option<usize> {
    let Some(idx) = scan_space(input, pos) else {
        return None;
    };
    let Some(idx) = scan_name(input, idx) else {
        return None;
    };
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };
    let Some(idx) = scan_att_type(input, idx) else {
        return None;
    };
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };

    scan_default_decl(input, idx, opts)
}

#[must_use]
const fn scan_att_type(input: &[u8], pos: usize) -> Option<usize> {
    if let Some(peek_idx) = peek_ch!(input, pos, 'C', 'D', 'A', 'T', 'A') {
        return Some(peek_idx);
    };

    if let Some(peek_idx) = peek_ch!(input, pos, 'I', 'D') {
        if let Some(peek_idx) = peek_ch!(input, peek_idx, 'R', 'E', 'F') {
            if let Some(peek_idx) = peek_ch!(input, peek_idx, 'S') {
                return Some(peek_idx);
            };
            return Some(peek_idx);
        };
        return Some(peek_idx);
    };

    if let Some(peek_idx) = peek_ch!(input, pos, 'E', 'N', 'T', 'I', 'T') {
        if let Some(peek_idx) = peek_ch!(input, peek_idx, 'Y') {
            return Some(peek_idx);
        };

        if let Some(peek_idx) = peek_ch!(input, pos, 'I', 'E', 'S') {
            return Some(peek_idx);
        };
    }

    if let Some(peek_idx) = peek_ch!(input, pos, 'N', 'M', 'T', 'O', 'K', 'E', 'N') {
        if let Some(peek_idx) = peek_ch!(input, peek_idx, 'S') {
            return Some(peek_idx);
        };
        return Some(peek_idx);
    };

    scan_enumerated_type(input, pos)
}

#[must_use]
const fn scan_enumerated_type(input: &[u8], pos: usize) -> Option<usize> {
    if let Some(peek_idx) = scan_notation_type(input, pos) {
        return Some(peek_idx);
    }

    if let Some(peek_idx) = scan_enumeration(input, pos) {
        return Some(peek_idx);
    }

    None
}

#[must_use]
const fn scan_notation_type(input: &[u8], pos: usize) -> Option<usize> {
    let idx = expect_ch!(input, pos, 'N', 'O', 'T', 'A', 'T', 'I', 'O', 'N');

    let Some(idx) = scan_space(input, idx) else {
        return None;
    };

    let idx = expect_ch!(input, idx, '(');

    let idx = scan_optional_space(input, idx);

    let Some(mut idx) = scan_name(input, idx) else {
        return None;
    };

    loop {
        let names_idx = scan_optional_space(input, idx);

        let Some(names_idx) = peek_ch!(input, names_idx, '|') else {
            break;
        };

        let names_idx = scan_optional_space(input, names_idx);

        let Some(peek_idx) = scan_name(input, names_idx) else {
            break;
        };

        idx = peek_idx;
    }

    let idx = scan_optional_space(input, idx);

    Some(expect_ch!(input, idx, ')'))
}

#[must_use]
const fn scan_enumeration(input: &[u8], pos: usize) -> Option<usize> {
    let idx = expect_ch!(input, pos, '(');

    let idx = scan_optional_space(input, idx);

    let Some(mut idx) = scan_nm_token(input, idx) else {
        return None;
    };

    loop {
        let nmtokens_idx = scan_optional_space(input, idx);

        let Some(nmtokens_idx) = peek_ch!(input, nmtokens_idx, '|') else {
            break;
        };

        let nmtokens_idx = scan_optional_space(input, nmtokens_idx);

        let Some(peek_idx) = scan_nm_token(input, nmtokens_idx) else {
            break;
        };
        idx = peek_idx;
    }

    let idx = scan_optional_space(input, idx);

    Some(expect_ch!(input, idx, ')'))
}

#[must_use]
const fn scan_default_decl(
    input: &[u8],
    pos: usize,
    opts: ScanAttributeValueOpts,
) -> Option<usize> {
    if let Some(peek_idx) = peek_ch!(input, pos, '#', 'R', 'E', 'Q', 'U', 'I', 'R', 'E', 'D') {
        return Some(peek_idx);
    }
    if let Some(peek_idx) = peek_ch!(input, pos, '#', 'I', 'M', 'P', 'L', 'I', 'E', 'D') {
        return Some(peek_idx);
    }

    let mut idx = pos;

    if let Some(peek_idx) = peek_ch!(input, idx, '#', 'F', 'I', 'X', 'E', 'D') {
        let Some(peek_idx) = scan_space(input, peek_idx) else {
            return None;
        };
        idx = peek_idx;
    }

    scan_attribute_value(input, idx, opts)
}

#[must_use]
const fn scan_char_ref_after_prefix(input: &[u8], pos: usize) -> Option<usize> {
    debug_assert!(input[pos - 2] == b'&');
    debug_assert!(input[pos - 1] == b'#');

    if input.len() <= pos {
        return None;
    }
    let byte = input[pos];
    let mut idx = pos;

    if byte == b'x' {
        idx += 1;

        if input.len() <= idx {
            return None;
        }
        let byte = input[pos];

        if !byte.is_ascii_hexdigit() {
            return None;
        }
        idx += 1;

        loop {
            if input.len() <= idx {
                return None;
            }

            match input[idx] {
                b';' => return Some(idx + 1),
                b if b.is_ascii_hexdigit() => {
                    idx += 1;
                }
                _ => {
                    return None;
                }
            }
        }
    } else {
        if !byte.is_ascii_digit() {
            return None;
        }
        idx += 1;

        loop {
            if input.len() <= idx {
                return None;
            }

            match input[idx] {
                b';' => return Some(idx + 1),
                b if b.is_ascii_digit() => {
                    idx += 1;
                }
                _ => {
                    return None;
                }
            }
        }
    }
}

#[cfg(test)]
#[must_use]
const fn scan_ref(input: &[u8], pos: usize) -> Option<usize> {
    if input.len() <= pos {
        return None;
    }

    let byte = input[pos];

    if byte != b'&' {
        return None;
    }

    scan_ref_after_ampersand(input, pos + 1)
}

#[must_use]
const fn scan_ref_after_ampersand(input: &[u8], pos: usize) -> Option<usize> {
    debug_assert!(input[pos - 1] == b'&');

    if input.len() <= pos {
        return None;
    }

    let byte = input[pos];

    if byte == b'#' {
        scan_char_ref_after_prefix(input, pos + 1)
    } else {
        scan_entity_ref_after_prefix(input, pos)
    }
}

#[inline]
#[must_use]
const fn scan_entity_ref_after_prefix(input: &[u8], pos: usize) -> Option<usize> {
    debug_assert!(input[pos - 1] == b'&');

    let Some(idx) = scan_name(input, pos) else {
        return None;
    };

    if input.len() <= idx {
        return None;
    }
    if input[idx] != b';' {
        return None;
    }
    Some(idx + 1)
}

/// Scan parameter-entity reference
#[must_use]
const fn scan_pe_ref(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Can optimize because the first character may have been peeked at

    let idx = expect_ch!(input, pos, '%');
    let Some(idx) = scan_name(input, idx) else {
        return None;
    };

    Some(expect_ch!(input, idx, ';'))
}

#[must_use]
const fn scan_entity_decl(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Should peek at the first character and decide what to do

    if let Some(idx) = scan_ge_decl(input, pos) {
        return Some(idx);
    }

    if let Some(idx) = scan_pe_decl(input, pos) {
        return Some(idx);
    }

    None
}

#[must_use]
const fn scan_ge_decl(input: &[u8], pos: usize) -> Option<usize> {
    let idx = expect_ch!(input, pos, '<', '!', 'E', 'N', 'T', 'I', 'T', 'Y');
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };
    let Some(idx) = scan_name(input, idx) else {
        return None;
    };
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };
    let Some(idx) = scan_entity_def(input, idx) else {
        return None;
    };
    let idx = scan_optional_space(input, idx);

    Some(expect_ch!(input, idx, '>'))
}

#[must_use]
const fn scan_pe_decl(input: &[u8], pos: usize) -> Option<usize> {
    let idx = expect_ch!(input, pos, '<', '!', 'E', 'N', 'T', 'I', 'T', 'Y');
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };
    let idx = expect_ch!(input, idx, '%');
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };
    let Some(idx) = scan_name(input, idx) else {
        return None;
    };
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };
    let Some(idx) = scan_pe_def(input, idx) else {
        return None;
    };
    let idx = scan_optional_space(input, idx);

    Some(expect_ch!(input, idx, '>'))
}

#[must_use]
const fn scan_entity_def(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Should peek at the first character and decide what to do

    if let Some(idx) = scan_entity_value(input, pos) {
        return Some(idx);
    }

    let Some(idx) = scan_external_id(input, pos) else {
        return None;
    };

    if let Some(peek_idx) = scan_ndata_decl(input, idx) {
        return Some(peek_idx);
    }

    Some(idx)
}

#[must_use]
const fn scan_pe_def(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Should peek at the first character and decide what to do

    if let Some(peek_idx) = scan_entity_value(input, pos) {
        return Some(peek_idx);
    }

    scan_external_id(input, pos)
}

#[must_use]
const fn scan_external_id(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Should peek at the first character and decide what to do

    if let Some(peek_idx) = peek_ch!(input, pos, 'S', 'Y', 'S', 'T', 'E', 'M') {
        let Some(peek_idx) = scan_space(input, peek_idx) else {
            return None;
        };
        return scan_system_literal(input, peek_idx);
    }

    if let Some(peek_idx) = peek_ch!(input, pos, 'P', 'U', 'B', 'L', 'I', 'C') {
        let Some(peek_idx) = scan_space(input, peek_idx) else {
            return None;
        };
        let Some(peek_idx) = scan_pub_id_literal(input, peek_idx) else {
            return None;
        };
        let Some(peek_idx) = scan_space(input, peek_idx) else {
            return None;
        };
        return scan_system_literal(input, peek_idx);
    }

    None
}

#[must_use]
const fn scan_ndata_decl(input: &[u8], pos: usize) -> Option<usize> {
    let Some(idx) = scan_space(input, pos) else {
        return None;
    };
    let idx = expect_ch!(input, idx, 'N', 'D', 'A', 'T', 'A');
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };
    scan_name(input, idx)
}

#[cfg(test)]
#[must_use]
const fn scan_encoding_decl(input: &[u8], pos: usize) -> Option<usize> {
    let Some(idx) = scan_space(input, pos) else {
        return None;
    };

    let idx = expect_ch!(input, idx, 'e', 'n', 'c', 'o', 'd', 'i', 'n', 'g');
    let Some(idx) = scan_eq(input, idx) else {
        return None;
    };
    let (quote_ch, idx) = expect_ch!(input, idx);

    if quote_ch != '"' && quote_ch != '\'' {
        // TODO: Allow no quotes?
        return None;
    }

    let (enc_name_first_ch, mut idx) = expect_ch!(input, idx);

    if !enc_name_first_ch.is_ascii_alphabetic() {
        return None;
    }

    loop {
        let (ch, peek_idx) = expect_ch!(input, idx);

        if ch == quote_ch {
            return Some(peek_idx);
        }

        if !is_enc_name_char(ch) {
            return None;
        }

        idx = peek_idx;
    }
}

#[cfg(test)]
#[must_use]
const fn is_enc_name_char(ch: char) -> bool {
    matches!(ch,
    'A'..='Z' | 'a'..='z' | '0'..='9' | '.' | '_' | '-'
    )
}

#[must_use]
const fn scan_notiation_decl(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Can optimize because the leading characters may have been peeked at

    let idx = expect_ch!(input, pos, '<', '!', 'N', 'O', 'T', 'A', 'T', 'I', 'O', 'N');
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };
    let Some(idx) = scan_name(input, idx) else {
        return None;
    };
    let Some(mut idx) = scan_space(input, idx) else {
        return None;
    };

    // TODO: Can peek at character and look
    if let Some(peek_idx) = scan_external_id(input, idx) {
        idx = peek_idx;
    } else if let Some(peek_idx) = scan_public_id(input, idx) {
        idx = peek_idx;
    } else {
        return None;
    }

    let idx = scan_optional_space(input, idx);

    Some(expect_ch!(input, idx, '>'))
}

#[must_use]
const fn scan_public_id(input: &[u8], pos: usize) -> Option<usize> {
    let idx = expect_ch!(input, pos, 'P', 'U', 'B', 'L', 'I', 'C');
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };

    scan_pub_id_literal(input, idx)
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use super::*;

    #[cfg(feature = "std")]
    proptest! {
        #[test]
        fn test_next_ch(expected in any::<char>()) {
            let mut buf = [0u8; 4];
            let expect_str = expected.encode_utf8(&mut buf);
            let expected_len = expect_str.len();
            let actual = next_ch(&buf, 0);
            assert_eq!(Some((expected, expected_len)), actual);
        }
    }

    #[test]
    fn test_scan_space() {
        let input = "a   \t  b";
        assert_eq!(None, scan_space(input.as_bytes(), 0));
        assert_eq!(Some(7), scan_space(input.as_bytes(), 1));
    }

    #[test]
    fn test_scan_char_data() {
        let input = " abcd";
        assert_eq!(
            input.len(),
            scan_char_data(input.as_bytes(), 0, ScanCharDataOpts::default())
        );

        let input = " abcd]]";
        assert_eq!(
            input.len(),
            scan_char_data(input.as_bytes(), 0, ScanCharDataOpts::default())
        );

        let input = " abcd]>";
        assert_eq!(
            input.len(),
            scan_char_data(input.as_bytes(), 0, ScanCharDataOpts::default())
        );

        let input = " abcd]]>";
        assert_eq!(
            input.len() - 3,
            scan_char_data(input.as_bytes(), 0, ScanCharDataOpts::default())
        );

        let input = " abcd]]]>";
        assert_eq!(
            input.len() - 3,
            scan_char_data(input.as_bytes(), 0, ScanCharDataOpts::default())
        );
    }

    #[test]
    fn test_scan_comment() {
        let input = "<!--a--> ";
        assert_eq!(
            Some(input.len() - 1),
            scan_comment(input.as_bytes(), 0, ScanCommentOpts::default())
        );

        let input = " <!--a--> ";
        assert_eq!(
            None,
            scan_comment(input.as_bytes(), 0, ScanCommentOpts::default())
        );

        let input = " <!--a--> ";
        assert_eq!(
            Some(input.len() - 1),
            scan_comment(input.as_bytes(), 1, ScanCommentOpts::default())
        );

        let input = " <!----> ";
        assert_eq!(
            Some(input.len() - 1),
            scan_comment(input.as_bytes(), 1, ScanCommentOpts::default())
        );

        let input = " <!-- -a --> ";
        assert_eq!(
            Some(input.len() - 1),
            scan_comment(input.as_bytes(), 1, ScanCommentOpts::default())
        );

        let input = " <!-- -a---> ";
        assert_eq!(
            None,
            scan_comment(input.as_bytes(), 1, ScanCommentOpts::default())
        );
        let input = " <!-- -a---> ";
        assert_eq!(
            Some(input.len() - 1),
            scan_comment(
                input.as_bytes(),
                1,
                ScanCommentOpts {
                    allow_double_dash: true,
                    ..Default::default()
                }
            )
        );

        let input = " <!-----> ";
        assert_eq!(
            None,
            scan_comment(input.as_bytes(), 1, ScanCommentOpts::default())
        );

        let input = " <!--- --> ";
        assert_eq!(
            Some(input.len() - 1),
            scan_comment(input.as_bytes(), 1, ScanCommentOpts::default())
        );

        let input = " <!-- - --> ";
        assert_eq!(
            Some(input.len() - 1),
            scan_comment(input.as_bytes(), 1, ScanCommentOpts::default())
        );

        let input = " <!-- example for <head> & <body> --> ";
        assert_eq!(
            Some(input.len() - 1),
            scan_comment(input.as_bytes(), 1, ScanCommentOpts::default())
        );

        let input = " <!----> ";
        assert_eq!(
            Some(input.len() - 1),
            scan_comment(input.as_bytes(), 1, ScanCommentOpts::default())
        );

        let input = "<!---->";
        assert_eq!(
            Some(input.len()),
            scan_comment(input.as_bytes(), 0, ScanCommentOpts::default())
        );

        let input = " <!---> ";
        assert_eq!(
            None,
            scan_comment(input.as_bytes(), 1, ScanCommentOpts::default())
        );

        let input = "<!-";
        assert_eq!(
            None,
            scan_comment(input.as_bytes(), 0, ScanCommentOpts::default())
        );
    }

    #[test]
    fn test_attribute_opts() {
        let input = r#"<id attr="1" id=test>"#;
        assert_eq!(
            Some(input.len()),
            scan_start_tag(input.as_bytes(), 0, ScanStartTagOpts::new_compatible())
        );
    }

    #[test]
    fn test_pi() {
        let input = r" <?xml?> ";
        assert_eq!(
            None,
            scan_pi(
                input.as_bytes(),
                1,
                ScanProcessingInstructionOpts::default()
            )
        );

        let input = r" <?xml-stylesheet example test ? > ?> ";
        assert_eq!(
            Some(input.len() - 1),
            scan_pi(
                input.as_bytes(),
                1,
                ScanProcessingInstructionOpts::default()
            )
        );
    }

    #[test]
    fn test_cd_sect() {
        let input = r" <![CDATA[]]> ";
        assert_eq!(
            Some(input.len() - 1),
            scan_cd_sect(input.as_bytes(), 1, ScanCdataSectionOpts::default())
        );

        let input = r" <![CDATA[]]]]> ";
        assert_eq!(
            Some(input.len() - 1),
            scan_cd_sect(input.as_bytes(), 1, ScanCdataSectionOpts::default())
        );

        let input = r" <![CDATA[<example>Hello</example>]]> ";
        assert_eq!(
            Some(input.len() - 1),
            scan_cd_sect(input.as_bytes(), 1, ScanCdataSectionOpts::default())
        );
    }

    #[test]
    fn test_xml_decl() {
        let input = r#"<?xml version="1.1"?>"#;
        assert_eq!(Some(input.len()), scan_xml_decl(input.as_bytes(), 0));

        let input = r#"<?xml version="1.1" standalone="yes"?>"#;
        assert_eq!(Some(input.len()), scan_xml_decl(input.as_bytes(), 0));

        let input = r#"<?xml version="1.1" encoding="UTF-8" standalone="yes"?>"#;
        assert_eq!(Some(input.len()), scan_xml_decl(input.as_bytes(), 0));
    }

    #[test]
    fn test_element_decl() {
        let input = r" <!ELEMENT example EMPTY> ";
        assert_eq!(
            Some(input.len() - 1),
            scan_element_decl(input.as_bytes(), 1)
        );

        let input = r" <!ELEMENT example ANY> ";
        assert_eq!(
            Some(input.len() - 1),
            scan_element_decl(input.as_bytes(), 1)
        );

        let input = r" <!ELEMENT test (#PCDATA)> ";
        assert_eq!(
            Some(input.len() - 1),
            scan_element_decl(input.as_bytes(), 1)
        );

        let input = r" <!ELEMENT x (#PCDATA|abcd)* > ";
        assert_eq!(
            Some(input.len() - 1),
            scan_element_decl(input.as_bytes(), 1)
        );

        let input = r" <!ELEMENT x (a, b, cdef?)> ";
        assert_eq!(
            Some(input.len() - 1),
            scan_element_decl(input.as_bytes(), 1)
        );

        let input = r" <!ELEMENT x (a, (b | c | d)*, efg*)> ";
        assert_eq!(
            Some(input.len() - 1),
            scan_element_decl(input.as_bytes(), 1)
        );

        // The parameter entity references are not allowed in internal (inline)
        // declaration
        let input = r" <!ELEMENT %nm.parm; %cnt.p; > ";
        assert_eq!(None, scan_element_decl(input.as_bytes(), 1));

        // The parameter entity references are not allowed in internal (inline)
        // declaration
        let input = r" <!ELEMENT x (%a.b; | %c.d;)*> ";
        assert_eq!(None, scan_element_decl(input.as_bytes(), 1));
    }

    #[test]
    fn test_read_resources() {
        let large_1_xml = include_str!("../../tests/resources/large-1.xml");
        assert_eq!(
            Some(large_1_xml.len()),
            scan_document(large_1_xml.as_bytes(), 0, ScanDocumentOpts::default())
        );

        let rss_1_xml = include_str!("../../tests/resources/rss-1.xml");
        assert_eq!(
            Some(rss_1_xml.len()),
            scan_document(rss_1_xml.as_bytes(), 0, ScanDocumentOpts::default())
        );

        let simple_1_xml = include_str!("../../tests/resources/simple-1.xml");
        assert_eq!(
            Some(simple_1_xml.len()),
            scan_document(simple_1_xml.as_bytes(), 0, ScanDocumentOpts::default())
        );

        let svg_1_xml = include_str!("../../tests/resources/svg-1.xml");
        assert_eq!(
            Some(svg_1_xml.len()),
            scan_document(svg_1_xml.as_bytes(), 0, ScanDocumentOpts::default())
        );
    }
}
