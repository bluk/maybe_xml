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

    if first < 0b1110_0000 {
        let code_pt = (((first & 0b0001_1111) as u32) << 6) | ((second & 0b0011_1111) as u32);
        match char::from_u32(code_pt) {
            Some(ch) => return Some((ch, index)),
            None => return None,
        }
    }

    let third = next_byte!();

    if first < 0b1111_0000 {
        let code_pt = (((first & 0b0001_1111) as u32) << 12)
            | (((second & 0b0011_1111) as u32) << 6)
            | ((third & 0b0011_1111) as u32);
        match char::from_u32(code_pt) {
            Some(ch) => return Some((ch, index)),
            None => return None,
        }
    }

    let fourth = next_byte!();

    let code_pt = (((first & 0b0001_1111) as u32) << 18)
        | (((second & 0b0011_1111) as u32) << 12)
        | (((third & 0b0011_1111) as u32) << 6)
        | ((fourth & 0b0011_1111) as u32);

    match char::from_u32(code_pt) {
        Some(ch) => Some((ch, index)),
        None => None,
    }
}

macro_rules! expect_ch {
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
        if let Some(index) = expect_ch!($input, $pos, $expected) {
            expect_ch!($input, index, $($expected_rem),+)
        } else {
            None
        }
    };
    ($input:expr, $pos:expr, $expected:expr) => {
        if let Some((ch, index)) = next_ch($input, $pos) {
            if $expected(ch) {
                Some(index)
            } else {
                None
            }
        } else {
            None
        }
    };
}

#[must_use]
const fn is_char(ch: char) -> bool {
    matches!(ch, 
      '\u{1}'..='\u{D7FF}'
      | '\u{E000}'..='\u{FFFD}'
      | '\u{1_0000}'..='\u{10_FFFF}')
}

#[must_use]
const fn is_restricted_char(ch: char) -> bool {
    matches!(ch, 
      '\u{1}'..='\u{8}'
      | '\u{B}'..='\u{C}'
      | '\u{E}'..='\u{1F}'
      | '\u{7F}'..='\u{84}'
      | '\u{86}'..='\u{9F}')
}

#[inline]
#[must_use]
const fn is_space(ch: char) -> bool {
    matches!(ch, '\u{20}' | '\u{9}' | '\u{D}' | '\u{A}')
}

#[must_use]
const fn scan_space(input: &[u8], pos: usize) -> Option<usize> {
    let Some((first_ch, mut idx)) = next_ch(input, pos) else {
        return None;
    };

    if !is_space(first_ch) {
        return None;
    }

    loop {
        let Some((ch, peek_idx)) = next_ch(input, idx) else {
            return Some(idx);
        };

        if !is_space(ch) {
            return Some(idx);
        }

        idx = peek_idx;
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
    let Some(mut idx) = expect_ch!(input, pos, is_name_start_char) else {
        return None;
    };

    loop {
        let Some(peek_idx) = expect_ch!(input, idx, is_name_ch) else {
            return Some(idx);
        };

        idx = peek_idx;
    }
}

#[must_use]
const fn scan_names(input: &[u8], pos: usize) -> Option<usize> {
    let Some(mut idx) = scan_name(input, pos) else {
        return None;
    };

    loop {
        let Some(peek_idx) = expect_ch!(input, idx, '\u{20}') else {
            return Some(idx);
        };

        let Some(peek_idx) = scan_name(input, peek_idx) else {
            return Some(idx);
        };

        idx = peek_idx;
    }
}

#[must_use]
const fn scan_nm_token(input: &[u8], pos: usize) -> Option<usize> {
    let Some(mut idx) = expect_ch!(input, pos, is_name_ch) else {
        return None;
    };

    loop {
        let Some(peek_idx) = expect_ch!(input, idx, is_name_ch) else {
            return Some(idx);
        };

        idx = peek_idx;
    }
}

#[must_use]
const fn scan_nm_tokens(input: &[u8], pos: usize) -> Option<usize> {
    let Some(mut idx) = scan_nm_token(input, pos) else {
        return None;
    };

    loop {
        let Some(peek_idx) = expect_ch!(input, idx, '\u{20}') else {
            return Some(idx);
        };

        let Some(peek_idx) = scan_nm_token(input, peek_idx) else {
            return Some(idx);
        };

        idx = peek_idx;
    }
}

#[must_use]
const fn scan_entity_value(input: &[u8], pos: usize) -> Option<usize> {
    let Some((quote_ch, mut idx)) = next_ch(input, pos) else {
        return None;
    };

    if quote_ch != '"' && quote_ch != '\'' {
        return None;
    }

    loop {
        let Some((ch, peek_idx)) = next_ch(input, idx) else {
            return None;
        };

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
            if let Some(peek_idx) = scan_ref(input, idx) {
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
    let Some((quote_ch, mut idx)) = next_ch(input, pos) else {
        return None;
    };

    debug_assert!(!is_space(quote_ch));

    if quote_ch != '"' && quote_ch != '\'' {
        if opts.allow_no_quote {
            loop {
                let Some((ch, peek_idx)) = next_ch(input, idx) else {
                    return None;
                };

                if is_space(ch) || ch == '>' {
                    return Some(idx);
                }

                if ch == '<' && !opts.allow_less_than {
                    return None;
                }

                if ch == '&' {
                    if let Some(peek_idx) = scan_ref(input, idx) {
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
        let Some((ch, peek_idx)) = next_ch(input, idx) else {
            return None;
        };

        if ch == quote_ch {
            return Some(peek_idx);
        }

        if ch == '<' && !opts.allow_less_than {
            return None;
        }

        if ch == '&' {
            if let Some(peek_idx) = scan_ref(input, idx) {
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
    let Some((quote_ch, mut idx)) = next_ch(input, pos) else {
        return None;
    };

    if quote_ch != '"' && quote_ch != '\'' {
        return None;
    }

    loop {
        let Some((ch, peek_idx)) = next_ch(input, idx) else {
            return None;
        };

        if ch == quote_ch {
            return Some(peek_idx);
        }

        idx = peek_idx;
    }
}

#[must_use]
const fn scan_pub_id_literal(input: &[u8], pos: usize) -> Option<usize> {
    let Some((quote_ch, mut idx)) = next_ch(input, pos) else {
        return None;
    };

    if quote_ch != '"' && quote_ch != '\'' {
        return None;
    }

    loop {
        let Some((ch, peek_idx)) = next_ch(input, idx) else {
            return None;
        };
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
#[must_use]
pub(crate) const fn scan_char_data(
    input: &[u8],
    pos: usize,
    opts: ScanCharDataOpts,
) -> Option<usize> {
    // TODO: Pass in peeked first character so that it does not need to be read again

    let Some((ch, mut idx)) = next_ch(input, pos) else {
        return None;
    };

    if ch == '<' {
        return None;
    }

    if ch == '&' && !opts.allow_ampersand {
        return None;
    }

    loop {
        let Some((ch, peek_idx)) = next_ch(input, idx) else {
            return Some(idx);
        };

        if ch == '<' {
            return Some(idx);
        }

        if ch == '&' && !opts.allow_ampersand {
            return Some(idx);
        }

        if ch == '>'
            && pos <= idx - 2
            && input[idx - 1] == b']'
            && input[idx - 2] == b']'
            && !opts.allow_cdata_section_close
        {
            return Some(idx - 2);
        }

        idx = peek_idx;
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct ScanCommentOpts {
    pub(crate) allow_double_dash: bool,
}

impl ScanCommentOpts {
    pub(crate) const fn new_compatible() -> Self {
        Self {
            allow_double_dash: true,
        }
    }
}

/// Scans for a comment.
#[must_use]
pub(crate) const fn scan_comment(input: &[u8], pos: usize, opts: ScanCommentOpts) -> Option<usize> {
    // TODO: Can optimize because the leading characters may have been peeked at

    let Some(mut idx) = expect_ch!(input, pos, '<', '!', '-', '-') else {
        return None;
    };

    let mut prev_ch = ' ';

    loop {
        loop {
            let Some((ch, peek_idx)) = next_ch(input, idx) else {
                return None;
            };

            // TODO: Have option to not care about if is_char
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

        if let Some(peek_idx) = expect_ch!(input, idx, '>') {
            return Some(peek_idx);
        } else if !opts.allow_double_dash {
            return None;
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
///
/// # Spec Divergence
///
/// - Process the prolog as a processing instruction
/// - Allow all characters (instead of just valid XML characters)
/// - Allow non spaces after the PI target name
#[must_use]
pub(crate) const fn scan_processing_instruction(
    input: &[u8],
    pos: usize,
    opts: ScanProcessingInstructionOpts,
) -> Option<usize> {
    // TODO: Can optimize because the leading characters may have been peeked at
    let Some(idx) = expect_ch!(input, pos, '<', '?') else {
        return None;
    };

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

    let mut scanned_space = false;
    if let Some(peek_idx) = scan_space(input, idx) {
        scanned_space = true;
        idx = peek_idx;
    }

    if !scanned_space {
        if let Some(peek_idx) = expect_ch!(input, idx, '?') {
            if let Some(peek_idx) = expect_ch!(input, peek_idx, '>') {
                return Some(peek_idx);
            };
        }

        if !opts.allow_non_space_after_pi_target {
            return None;
        }
    }

    loop {
        let Some((ch, peek_idx)) = next_ch(input, idx) else {
            return None;
        };

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
///
/// # Spec Divergence
///
/// - Allow all characters (instead of just valid XML characters)
#[must_use]
pub(crate) const fn scan_cdata_section(
    input: &[u8],
    pos: usize,
    opts: ScanCdataSectionOpts,
) -> Option<usize> {
    // TODO: Can optimize because the leading characters may have been peeked at

    let Some(mut idx) = expect_ch!(input, pos, '<', '!', '[', 'C', 'D', 'A', 'T', 'A', '[') else {
        return None;
    };

    loop {
        let Some((ch, peek_idx)) = next_ch(input, idx) else {
            return None;
        };

        if !opts.allow_all_chars && !is_char(ch) {
            return None;
        }

        if ch == '>' && pos <= idx - 2 && input[idx - 1] == b']' && input[idx - 2] == b']' {
            return Some(peek_idx);
        }

        idx = peek_idx;
    }
}

#[must_use]
pub(crate) const fn scan_prolog(
    input: &[u8],
    pos: usize,
    opts: ScanMarkupDeclOpts,
) -> Option<usize> {
    let Some(mut idx) = scan_xml_decl(input, pos) else {
        return None;
    };

    loop {
        let Some(peek_idx) = scan_misc(
            input,
            idx,
            ScanMiscOpts {
                comment_opts: opts.comment_opts,
                pi_opts: opts.pi_opts,
            },
        ) else {
            break;
        };
        idx = peek_idx;
    }

    if let Some(peek_idx) = scan_doctype_decl(input, idx, opts) {
        idx = peek_idx;

        loop {
            let Some(peek_idx) = scan_misc(
                input,
                idx,
                ScanMiscOpts {
                    comment_opts: opts.comment_opts,
                    pi_opts: opts.pi_opts,
                },
            ) else {
                break;
            };
            idx = peek_idx;
        }
    }

    Some(idx)
}

#[must_use]
const fn scan_xml_decl(input: &[u8], pos: usize) -> Option<usize> {
    let Some(idx) = expect_ch!(input, pos, '<', '?', 'x', 'm', 'l') else {
        return None;
    };
    let Some(mut idx) = scan_version_info(input, idx) else {
        return None;
    };

    if let Some(peek_idx) = scan_encoding_decl(input, idx) {
        idx = peek_idx;
    }

    if let Some(peek_idx) = scan_sd_decl(input, idx) {
        idx = peek_idx;
    }

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    expect_ch!(input, idx, '?', '>')
}

#[must_use]
const fn scan_version_info(input: &[u8], pos: usize) -> Option<usize> {
    let Some(idx) = scan_space(input, pos) else {
        return None;
    };

    let Some(idx) = expect_ch!(input, idx, 'v', 'e', 'r', 's', 'i', 'o', 'n') else {
        return None;
    };
    let Some(idx) = scan_eq(input, idx) else {
        return None;
    };

    let Some((quote_ch, mut idx)) = next_ch(input, idx) else {
        return None;
    };

    if quote_ch != '"' && quote_ch != '\'' {
        // TODO: Add no quotes option?
        return None;
    }

    loop {
        let Some((ch, peek_idx)) = next_ch(input, idx) else {
            return None;
        };

        if ch == quote_ch {
            return Some(peek_idx);
        }

        idx = peek_idx;
    }
}

#[must_use]
const fn scan_eq(input: &[u8], pos: usize) -> Option<usize> {
    let mut idx = pos;
    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    };

    let Some(mut idx) = expect_ch!(input, idx, '=') else {
        return None;
    };

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    };

    Some(idx)
}

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct ScanMiscOpts {
    pub(crate) comment_opts: ScanCommentOpts,
    pub(crate) pi_opts: ScanProcessingInstructionOpts,
}

impl ScanMiscOpts {
    pub(crate) const fn new_compatible() -> Self {
        Self {
            comment_opts: ScanCommentOpts::new_compatible(),
            pi_opts: ScanProcessingInstructionOpts::new_compatible(),
        }
    }
}

#[must_use]
const fn scan_misc(input: &[u8], pos: usize, opts: ScanMiscOpts) -> Option<usize> {
    // TODO: Should peek at the first character and decide what to do

    if let Some(idx) = scan_comment(input, pos, opts.comment_opts) {
        return Some(idx);
    }

    if let Some(idx) = scan_processing_instruction(input, pos, opts.pi_opts) {
        return Some(idx);
    }

    if let Some(idx) = scan_space(input, pos) {
        return Some(idx);
    }

    None
}

#[must_use]
pub(crate) const fn scan_doctype_decl(
    input: &[u8],
    pos: usize,
    opts: ScanMarkupDeclOpts,
) -> Option<usize> {
    // TODO: Can optimize because the leading characters may have been peeked at

    let Some(idx) = expect_ch!(input, pos, '<', '!', 'D', 'O', 'C', 'T', 'Y', 'P', 'E') else {
        return None;
    };

    let Some(idx) = scan_space(input, idx) else {
        return None;
    };

    let Some(mut idx) = scan_name(input, idx) else {
        return None;
    };

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;

        if let Some(peek_idx) = scan_external_id(input, idx) {
            idx = peek_idx;

            if let Some(peek_idx) = scan_space(input, idx) {
                idx = peek_idx;
            }
        }
    }

    if let Some(peek_idx) = expect_ch!(input, idx, '[') {
        idx = scan_int_subset(input, peek_idx, opts);

        // XXX: Verify the ']' is not parsed by scan_int_subset

        let Some(peek_idx) = expect_ch!(input, idx, ']') else {
            return None;
        };
        idx = peek_idx;

        if let Some(peek_idx) = scan_space(input, idx) {
            idx = peek_idx;
        }
    }

    expect_ch!(input, idx, '>')
}

#[must_use]
const fn scan_decl_sep(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Should peek at the first character and decide what to do

    if let Some(idx) = scan_pe_ref(input, pos) {
        return Some(idx);
    }

    if let Some(idx) = scan_space(input, pos) {
        return Some(idx);
    }

    None
}

#[must_use]
const fn scan_int_subset(input: &[u8], pos: usize, opts: ScanMarkupDeclOpts) -> usize {
    // TODO: Should peek at the first character and decide what to do
    let mut idx = pos;

    loop {
        if let Some(peek_idx) = scan_markup_decl(input, idx, opts) {
            idx = peek_idx;
            continue;
        }

        if let Some(peek_idx) = scan_decl_sep(input, idx) {
            idx = peek_idx;
            continue;
        }

        break;
    }

    idx
}

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct ScanMarkupDeclOpts {
    pub(crate) comment_opts: ScanCommentOpts,
    pub(crate) pi_opts: ScanProcessingInstructionOpts,
    pub(crate) attr_value_opts: ScanAttributeValueOpts,
}

impl ScanMarkupDeclOpts {
    pub(crate) const fn new_compatible() -> Self {
        Self {
            comment_opts: ScanCommentOpts::new_compatible(),
            pi_opts: ScanProcessingInstructionOpts::new_compatible(),
            attr_value_opts: ScanAttributeValueOpts::new_compatible(),
        }
    }
}

#[must_use]
const fn scan_markup_decl(input: &[u8], pos: usize, opts: ScanMarkupDeclOpts) -> Option<usize> {
    // TODO: Should peek at the first character and decide what to do
    if let Some(idx) = scan_element_decl(input, pos) {
        return Some(idx);
    }

    if let Some(idx) = scan_att_list_decl(input, pos, opts.attr_value_opts) {
        return Some(idx);
    }

    if let Some(idx) = scan_entity_decl(input, pos) {
        return Some(idx);
    }

    if let Some(idx) = scan_notiation_decl(input, pos) {
        return Some(idx);
    }

    if let Some(idx) = scan_processing_instruction(input, pos, opts.pi_opts) {
        return Some(idx);
    }

    if let Some(idx) = scan_comment(input, pos, opts.comment_opts) {
        return Some(idx);
    }

    None
}

// XXX: Missing 30, 31

#[must_use]
const fn scan_sd_decl(input: &[u8], pos: usize) -> Option<usize> {
    let Some(idx) = scan_space(input, pos) else {
        return None;
    };

    let Some(idx) = expect_ch!(input, idx, 's', 't', 'a', 'n', 'd', 'a', 'l', 'o', 'n', 'e') else {
        return None;
    };
    let Some(idx) = scan_eq(input, idx) else {
        return None;
    };
    let Some((quote_ch, mut idx)) = next_ch(input, idx) else {
        return None;
    };

    if quote_ch != '"' && quote_ch != '\'' {
        // TODO: Add no quotes option?
        return None;
    }

    loop {
        let Some((ch, peek_idx)) = next_ch(input, idx) else {
            return None;
        };

        // TODO: Be stricter and only allow ('yes' | 'no')?

        if ch == quote_ch {
            return Some(peek_idx);
        }

        idx = peek_idx;
    }
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

    let Some(idx) = expect_ch!(input, pos, '<') else {
        return None;
    };
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
            if let Some(peek_idx) = expect_ch!(input, peek_idx, '/') {
                if expect_ch!(input, peek_idx, '>').is_some() {
                    return None;
                }

                idx = peek_idx;
                continue;
            }
        }

        break;
    }

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    expect_ch!(input, idx, '>')
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

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct ScanEndTagOpts {
    pub(crate) allow_attributes: bool,
    pub(crate) attr_opts: ScanAttributeOpts,
}

impl ScanEndTagOpts {
    pub(crate) const fn new_compatible() -> Self {
        Self {
            allow_attributes: true,
            attr_opts: ScanAttributeOpts::new_compatible(),
        }
    }
}

#[must_use]
pub(crate) const fn scan_end_tag(input: &[u8], pos: usize, opts: ScanEndTagOpts) -> Option<usize> {
    // TODO: Can optimize because the leading characters may have been peeked at

    let Some(idx) = expect_ch!(input, pos, '<', '/') else {
        return None;
    };
    let Some(mut idx) = scan_name(input, idx) else {
        return None;
    };

    if opts.allow_attributes {
        loop {
            let Some(peek_idx) = scan_space(input, idx) else {
                break;
            };
            let Some(peek_idx) = scan_attribute(input, peek_idx, opts.attr_opts) else {
                break;
            };
            idx = peek_idx;
        }
    }

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    expect_ch!(input, idx, '>')
}

// XXX: Skip 43

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

    let Some(idx) = expect_ch!(input, pos, '<') else {
        return None;
    };
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
            if let Some(peek_idx) = expect_ch!(input, peek_idx, '/') {
                if expect_ch!(input, peek_idx, '>').is_some() {
                    break;
                }

                idx = peek_idx;
                continue;
            }
        }

        break;
    }

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    expect_ch!(input, idx, '/', '>')
}

#[must_use]
pub(crate) const fn scan_element_decl(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Can optimize because the leading characters may have been peeked at
    let Some(idx) = expect_ch!(input, pos, '<', '!', 'E', 'L', 'E', 'M', 'E', 'N', 'T') else {
        return None;
    };
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };
    let Some(idx) = scan_name(input, idx) else {
        return None;
    };

    let Some(mut idx) = scan_content_spec(input, idx) else {
        return None;
    };

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    expect_ch!(input, idx, '>')
}

#[must_use]
pub(crate) const fn scan_content_spec(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Should peek to decide which branch

    if let Some(idx) = expect_ch!(input, pos, 'E', 'M', 'P', 'T', 'Y') {
        return Some(idx);
    };

    if let Some(idx) = expect_ch!(input, pos, 'A', 'N', 'Y') {
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
pub(crate) const fn scan_children(input: &[u8], pos: usize) -> Option<usize> {
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
pub(crate) const fn scan_cp(input: &[u8], pos: usize) -> Option<usize> {
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
pub(crate) const fn scan_choice(input: &[u8], pos: usize) -> Option<usize> {
    let Some(mut idx) = expect_ch!(input, pos, '(') else {
        return None;
    };
    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    let Some(peek_idx) = scan_cp(input, idx) else {
        return None;
    };
    idx = peek_idx;

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    let Some(peek_idx) = expect_ch!(input, idx, '|') else {
        return None;
    };
    idx = peek_idx;

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    let Some(peek_idx) = scan_cp(input, idx) else {
        return None;
    };
    idx = peek_idx;

    loop {
        let mut choices_idx = idx;

        if let Some(peek_idx) = scan_space(input, choices_idx) {
            choices_idx = peek_idx;
        }

        let Some(peek_idx) = expect_ch!(input, choices_idx, '|') else {
            break;
        };
        choices_idx = peek_idx;

        if let Some(peek_idx) = scan_space(input, choices_idx) {
            choices_idx = peek_idx;
        }

        let Some(peek_idx) = scan_cp(input, choices_idx) else {
            return None;
        };
        idx = peek_idx;
    }

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    expect_ch!(input, idx, ')')
}

#[must_use]
pub(crate) const fn scan_seq(input: &[u8], pos: usize) -> Option<usize> {
    let Some(mut idx) = expect_ch!(input, pos, '(') else {
        return None;
    };
    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    let Some(peek_idx) = scan_cp(input, idx) else {
        return None;
    };
    idx = peek_idx;

    loop {
        let mut choices_idx = idx;

        if let Some(peek_idx) = scan_space(input, choices_idx) {
            choices_idx = peek_idx;
        }

        let Some(peek_idx) = expect_ch!(input, choices_idx, ',') else {
            break;
        };
        choices_idx = peek_idx;

        if let Some(peek_idx) = scan_space(input, choices_idx) {
            choices_idx = peek_idx;
        }

        let Some(peek_idx) = scan_cp(input, choices_idx) else {
            return None;
        };
        idx = peek_idx;
    }

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    expect_ch!(input, idx, ')')
}

#[must_use]
pub(crate) const fn scan_mixed(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Could optimize if the leading character is peaked?

    let Some(mut idx) = expect_ch!(input, pos, '(') else {
        return None;
    };
    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    let Some(mut idx) = expect_ch!(input, idx, '#', 'P', 'C', 'D', 'A', 'T', 'A') else {
        return None;
    };

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    // Check for an early exit

    let Some((ch, peek_idx)) = next_ch(input, idx) else {
        return None;
    };
    if ch == ')' {
        let Some((ch, peek_2_idx)) = next_ch(input, peek_idx) else {
            return Some(peek_idx);
        };

        if ch == '*' {
            return Some(peek_2_idx);
        }

        return Some(peek_idx);
    }

    if ch != '|' {
        return None;
    }
    idx = peek_idx;

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    let Some(mut idx) = scan_name(input, idx) else {
        return None;
    };

    loop {
        if let Some(peek_idx) = scan_space(input, idx) {
            idx = peek_idx;
        }

        let Some((ch, peek_idx)) = next_ch(input, idx) else {
            return None;
        };
        if ch == ')' {
            let Some((ch, peek_2_idx)) = next_ch(input, peek_idx) else {
                return None;
            };

            if ch != '*' {
                return None;
            }

            return Some(peek_2_idx);
        }

        if ch != '|' {
            return None;
        }
        idx = peek_idx;

        if let Some(peek_idx) = scan_space(input, idx) {
            idx = peek_idx;
        }

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
    let Some(idx) = expect_ch!(input, pos, '<', '!', 'A', 'T', 'T', 'L', 'I', 'S', 'T') else {
        return None;
    };
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };
    let Some(mut idx) = scan_name(input, idx) else {
        return None;
    };

    while let Some(peek_idx) = scan_att_def(input, idx, opts) {
        idx = peek_idx;
    }

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    expect_ch!(input, idx, '>')
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
    if let Some(peek_idx) = expect_ch!(input, pos, 'C', 'D', 'A', 'T', 'A') {
        return Some(peek_idx);
    };

    if let Some(peek_idx) = expect_ch!(input, pos, 'I', 'D', 'R', 'E', 'F', 'S') {
        return Some(peek_idx);
    };

    if let Some(peek_idx) = expect_ch!(input, pos, 'I', 'D', 'R', 'E', 'F') {
        return Some(peek_idx);
    };

    if let Some(peek_idx) = expect_ch!(input, pos, 'I', 'D') {
        return Some(peek_idx);
    };

    if let Some(peek_idx) = expect_ch!(input, pos, 'E', 'N', 'T', 'I', 'T', 'I', 'E', 'S') {
        return Some(peek_idx);
    };

    if let Some(peek_idx) = expect_ch!(input, pos, 'E', 'N', 'T', 'I', 'T', 'Y') {
        return Some(peek_idx);
    };

    if let Some(peek_idx) = expect_ch!(input, pos, 'N', 'M', 'T', 'O', 'K', 'E', 'N', 'S') {
        return Some(peek_idx);
    };

    if let Some(peek_idx) = expect_ch!(input, pos, 'N', 'M', 'T', 'O', 'K', 'E', 'N') {
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
    let Some(idx) = expect_ch!(input, pos, 'N', 'O', 'T', 'A', 'T', 'I', 'O', 'N') else {
        return None;
    };

    let Some(idx) = scan_space(input, idx) else {
        return None;
    };

    let Some(mut idx) = expect_ch!(input, idx, '(') else {
        return None;
    };

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    let Some(mut idx) = scan_name(input, idx) else {
        return None;
    };

    loop {
        let mut names_idx = idx;

        if let Some(peek_idx) = scan_space(input, names_idx) {
            names_idx = peek_idx;
        }

        let Some(peek_idx) = expect_ch!(input, names_idx, '|') else {
            break;
        };
        names_idx = peek_idx;

        if let Some(peek_idx) = scan_space(input, names_idx) {
            names_idx = peek_idx;
        }

        let Some(peek_idx) = scan_name(input, names_idx) else {
            break;
        };

        idx = peek_idx;
    }

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    expect_ch!(input, idx, ')')
}

#[must_use]
const fn scan_enumeration(input: &[u8], pos: usize) -> Option<usize> {
    let Some(mut idx) = expect_ch!(input, pos, '(') else {
        return None;
    };
    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }
    let Some(mut idx) = scan_nm_token(input, idx) else {
        return None;
    };

    loop {
        let mut nmtokens_idx = idx;
        if let Some(peek_idx) = scan_space(input, nmtokens_idx) {
            nmtokens_idx = peek_idx;
        }

        let Some(peek_idx) = expect_ch!(input, nmtokens_idx, '|') else {
            break;
        };
        nmtokens_idx = peek_idx;

        if let Some(peek_idx) = scan_space(input, nmtokens_idx) {
            nmtokens_idx = peek_idx;
        }

        let Some(peek_idx) = scan_nm_token(input, nmtokens_idx) else {
            break;
        };
        idx = peek_idx;
    }

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    expect_ch!(input, idx, ')')
}

#[must_use]
const fn scan_default_decl(
    input: &[u8],
    pos: usize,
    opts: ScanAttributeValueOpts,
) -> Option<usize> {
    if let Some(peek_idx) = expect_ch!(input, pos, '#', 'R', 'E', 'Q', 'U', 'I', 'R', 'E', 'D') {
        return Some(peek_idx);
    }
    if let Some(peek_idx) = expect_ch!(input, pos, '#', 'I', 'M', 'P', 'L', 'I', 'E', 'D') {
        return Some(peek_idx);
    }

    let mut idx = pos;

    if let Some(peek_idx) = expect_ch!(input, idx, '#', 'F', 'I', 'X', 'E', 'D') {
        let Some(peek_idx) = scan_space(input, peek_idx) else {
            return None;
        };
        idx = peek_idx;
    }

    scan_attribute_value(input, idx, opts)
}

#[must_use]
const fn scan_char_ref(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Can optimize because the leading characters may have been peeked at

    let Some(idx) = expect_ch!(input, pos, '&', '#') else {
        return None;
    };

    let Some((ch, mut idx)) = next_ch(input, idx) else {
        return None;
    };

    if ch == 'x' {
        let Some((ch, mut idx)) = next_ch(input, idx) else {
            return None;
        };

        if !ch.is_ascii_hexdigit() {
            return None;
        }

        loop {
            let Some((ch, peek_idx)) = next_ch(input, idx) else {
                return None;
            };

            match ch {
                ';' => return Some(peek_idx),
                _ if ch.is_ascii_hexdigit() => {
                    idx = peek_idx;
                }
                _ => {
                    return None;
                }
            }
        }
    } else {
        if !ch.is_ascii_digit() {
            return None;
        }

        loop {
            let Some((ch, peek_idx)) = next_ch(input, idx) else {
                return None;
            };

            match ch {
                ';' => return Some(peek_idx),
                _ if ch.is_ascii_digit() => {
                    idx = peek_idx;
                }
                _ => {
                    return None;
                }
            }
        }
    }
}

#[must_use]
const fn scan_ref(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Can optimize because the first character may have been peeked at

    match next_ch(input, pos) {
        Some(('&', _)) => match next_ch(input, pos + 1) {
            Some(('#', _)) => scan_char_ref(input, pos),
            Some((_, _)) => scan_entity_ref(input, pos),
            None => None,
        },
        _ => None,
    }
}

#[must_use]
const fn scan_entity_ref(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Can optimize because the first character may have been peeked at

    let Some(idx) = expect_ch!(input, pos, '&') else {
        return None;
    };
    let Some(idx) = scan_name(input, idx) else {
        return None;
    };

    expect_ch!(input, idx, ';')
}

/// Scan parameter-entity reference
#[must_use]
const fn scan_pe_ref(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Can optimize because the first character may have been peeked at

    let Some(idx) = expect_ch!(input, pos, '%') else {
        return None;
    };

    let Some(idx) = scan_name(input, idx) else {
        return None;
    };

    expect_ch!(input, idx, ';')
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
    let Some(idx) = expect_ch!(input, pos, '<', '!', 'E', 'N', 'T', 'I', 'T', 'Y') else {
        return None;
    };
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };
    let Some(idx) = scan_name(input, idx) else {
        return None;
    };
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };
    let Some(mut idx) = scan_entity_def(input, idx) else {
        return None;
    };
    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    expect_ch!(input, idx, '>')
}

#[must_use]
const fn scan_pe_decl(input: &[u8], pos: usize) -> Option<usize> {
    let Some(idx) = expect_ch!(input, pos, '<', '!', 'E', 'N', 'T', 'I', 'T', 'Y') else {
        return None;
    };
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };
    let Some(idx) = expect_ch!(input, idx, '%') else {
        return None;
    };
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };
    let Some(idx) = scan_name(input, idx) else {
        return None;
    };
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };
    let Some(mut idx) = scan_pe_def(input, idx) else {
        return None;
    };
    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    expect_ch!(input, idx, '>')
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

    if let Some(idx) = scan_entity_value(input, pos) {
        return Some(idx);
    }

    scan_external_id(input, pos)
}

#[must_use]
const fn scan_external_id(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Should peek at the first character and decide what to do

    if let Some(idx) = expect_ch!(input, pos, 'S', 'Y', 'S', 'T', 'E', 'M') {
        let Some(idx) = scan_space(input, idx) else {
            return None;
        };
        return scan_system_literal(input, idx);
    }

    if let Some(idx) = expect_ch!(input, pos, 'P', 'U', 'B', 'L', 'I', 'C') {
        let Some(idx) = scan_space(input, idx) else {
            return None;
        };
        let Some(idx) = scan_pub_id_literal(input, idx) else {
            return None;
        };
        let Some(idx) = scan_space(input, idx) else {
            return None;
        };
        return scan_system_literal(input, idx);
    }

    None
}

#[must_use]
const fn scan_ndata_decl(input: &[u8], pos: usize) -> Option<usize> {
    let Some(idx) = scan_space(input, pos) else {
        return None;
    };
    let Some(idx) = expect_ch!(input, idx, 'N', 'D', 'A', 'T', 'A') else {
        return None;
    };
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };
    scan_name(input, idx)
}

#[must_use]
const fn scan_encoding_decl(input: &[u8], pos: usize) -> Option<usize> {
    let mut idx = pos;
    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    let Some(idx) = expect_ch!(input, idx, 'e', 'n', 'c', 'o', 'd', 'i', 'n', 'g') else {
        return None;
    };
    let Some(idx) = scan_eq(input, idx) else {
        return None;
    };
    let Some((quote_ch, idx)) = next_ch(input, idx) else {
        return None;
    };

    if quote_ch != '"' && quote_ch != '\'' {
        // TODO: Allow no quotes?
        return None;
    }

    let Some((enc_name_first_ch, mut idx)) = next_ch(input, idx) else {
        return None;
    };

    if !enc_name_first_ch.is_ascii_alphabetic() {
        return None;
    }

    loop {
        let Some((ch, peek_idx)) = next_ch(input, idx) else {
            return None;
        };
        idx = peek_idx;

        if ch == quote_ch {
            return Some(idx);
        }

        if !is_enc_name_char(ch) {
            return None;
        }
    }
}

#[must_use]
const fn is_enc_name_char(ch: char) -> bool {
    matches!(ch,
    'A'..='Z' | 'a'..='z' | '0'..='9' | '.' | '_' | '-'
    )
}

#[must_use]
pub(crate) const fn scan_notiation_decl(input: &[u8], pos: usize) -> Option<usize> {
    // TODO: Can optimize because the leading characters may have been peeked at

    let Some(idx) = expect_ch!(input, pos, '<', '!', 'N', 'O', 'T', 'A', 'T', 'I', 'O', 'N') else {
        return None;
    };
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

    if let Some(peek_idx) = scan_space(input, idx) {
        idx = peek_idx;
    }

    expect_ch!(input, idx, '>')
}

#[must_use]
const fn scan_public_id(input: &[u8], pos: usize) -> Option<usize> {
    let Some(idx) = expect_ch!(input, pos, 'P', 'U', 'B', 'L', 'I', 'C') else {
        return None;
    };
    let Some(idx) = scan_space(input, idx) else {
        return None;
    };

    scan_pub_id_literal(input, idx)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    const fn test_space() {
        assert!(is_space(' '));
        assert!(is_space('\t'));
        assert!(is_space('\r'));
        assert!(is_space('\n'));
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
            Some(input.len()),
            scan_char_data(input.as_bytes(), 0, ScanCharDataOpts::default())
        );

        let input = " abcd]]";
        assert_eq!(
            Some(input.len()),
            scan_char_data(input.as_bytes(), 0, ScanCharDataOpts::default())
        );

        let input = " abcd]>";
        assert_eq!(
            Some(input.len()),
            scan_char_data(input.as_bytes(), 0, ScanCharDataOpts::default())
        );

        let input = " abcd]]>";
        assert_eq!(
            Some(input.len() - 3),
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

        let input = " <!-----> ";
        assert_eq!(
            None,
            scan_comment(input.as_bytes(), 1, ScanCommentOpts::default())
        );

        let input = " <!-- - --> ";
        assert_eq!(
            Some(input.len() - 1),
            scan_comment(input.as_bytes(), 1, ScanCommentOpts::default())
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
    fn test_xml_decl() {
        let input = r#"<?xml version="1.1"?>"#;
        assert_eq!(Some(input.len()), scan_xml_decl(input.as_bytes(), 0));

        let input = r#"<?xml version="1.1" standalone="yes"?>"#;
        assert_eq!(Some(input.len()), scan_xml_decl(input.as_bytes(), 0));

        let input = r#"<?xml version="1.1" encoding="UTF-8" standalone="yes"?>"#;
        assert_eq!(Some(input.len()), scan_xml_decl(input.as_bytes(), 0));
    }
}
