// Copyright 2022 Bryant Luk
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! An internal byte slice reader.

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub(crate) enum QuoteState {
    None,
    Single,
    Double,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub(crate) struct BracketCount(pub(crate) u64);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum QuoteAndBracketContextAwareFoundState {
    NotFound(QuoteState, BracketCount, AlreadyFoundByteSeqCount),
    Found(usize),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum QuoteContextAwareFoundState {
    NotFound(QuoteState),
    Found(usize),
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub(crate) struct AlreadyFoundByteSeqCount(pub(crate) usize);

#[inline]
pub(crate) fn quote_context_aware_find(
    buf: &[u8],
    mut quote_state: QuoteState,
) -> QuoteContextAwareFoundState {
    if buf.is_empty() {
        return QuoteContextAwareFoundState::NotFound(quote_state);
    }

    let mut read = 0;

    let byte_seq = b">";
    let byte_seq_len = byte_seq.len();
    let last_expected_byte = byte_seq[byte_seq_len - 1];

    loop {
        let mut bytes = &buf[read..];
        let mut found_last_byte = false;

        for (index, byte) in bytes.iter().enumerate() {
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
                // if byte_seq ended with a quote character, this should not be a part of the match
                b if *b == last_expected_byte => match quote_state {
                    QuoteState::None => {
                        bytes = &bytes[..=index];
                        found_last_byte = true;
                        break;
                    }
                    QuoteState::Single | QuoteState::Double => {}
                },
                _ => {}
            }
        }
        read += bytes.len();

        if found_last_byte {
            debug_assert_eq!(quote_state, QuoteState::None);
            if byte_seq_len <= read && &buf[read - byte_seq_len..read] == byte_seq {
                debug_assert_eq!(
                    find_matching_suffix(byte_seq, &buf[read - byte_seq_len..read]),
                    AlreadyFoundByteSeqCount(byte_seq_len)
                );
                return QuoteContextAwareFoundState::Found(read);
            }
        } else {
            debug_assert_eq!(read, buf.len());
            return QuoteContextAwareFoundState::NotFound(quote_state);
        }
    }
}

#[inline]
pub(crate) fn peek(bytes: &[u8]) -> Option<u8> {
    bytes.first().copied()
}

#[inline]
pub(crate) fn peek2(bytes: &[u8]) -> Option<u8> {
    let pos = 1;
    if pos < bytes.len() {
        Some(bytes[pos])
    } else {
        None
    }
}

#[inline]
pub(crate) fn quote_and_bracket_context_aware_find(
    buf: &[u8],
    byte_seq: &[u8],
    already_found_byte_seq_count: AlreadyFoundByteSeqCount,
    quote_state: QuoteState,
    mut bracket_count: BracketCount,
) -> QuoteAndBracketContextAwareFoundState {
    let mut read = 0;

    if buf.is_empty() {
        return QuoteAndBracketContextAwareFoundState::NotFound(
            quote_state,
            bracket_count,
            already_found_byte_seq_count,
        );
    }

    match quote_state {
        QuoteState::None => {
            let prefix_check = &byte_seq[(already_found_byte_seq_count.0)..];
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
                        return QuoteAndBracketContextAwareFoundState::Found(read);
                    }
                    return QuoteAndBracketContextAwareFoundState::NotFound(
                        QuoteState::None,
                        bracket_count,
                        AlreadyFoundByteSeqCount(read + already_found_byte_seq_count.0),
                    );
                }
            }

            if read == prefix_check_len && bracket_count.0 == 0 {
                return QuoteAndBracketContextAwareFoundState::Found(read);
            }
        }
        QuoteState::Single | QuoteState::Double => {}
    }

    quote_and_bracket_context_aware_find_2(buf, byte_seq, quote_state, bracket_count, read)
}

#[inline]
fn quote_and_bracket_context_aware_find_2(
    buf: &[u8],
    byte_seq: &[u8],
    mut quote_state: QuoteState,
    mut bracket_count: BracketCount,
    mut read: usize,
) -> QuoteAndBracketContextAwareFoundState {
    let byte_seq_len = byte_seq.len();
    let last_expected_byte = byte_seq[byte_seq_len - 1];

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
                // if byte_seq ended with a quote character, this should not be a part of the match
                b if *b == last_expected_byte => {
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
                        if byte_seq_len <= read && &buf[read - byte_seq_len..read] == byte_seq {
                            debug_assert_eq!(
                                find_matching_suffix(byte_seq, &buf[read - byte_seq_len..read]),
                                AlreadyFoundByteSeqCount(byte_seq_len)
                            );
                            return QuoteAndBracketContextAwareFoundState::Found(read);
                        }
                    }
                    QuoteState::Double | QuoteState::Single => {}
                }
            }
        } else {
            debug_assert_eq!(read, buf.len());
            let already_found_byte_seq_count = match (bracket_count.0 == 0, quote_state) {
                (true, QuoteState::None) => find_matching_suffix(byte_seq, buf),
                _ => AlreadyFoundByteSeqCount(0),
            };
            return QuoteAndBracketContextAwareFoundState::NotFound(
                quote_state,
                bracket_count,
                already_found_byte_seq_count,
            );
        }
    }
}

#[inline]
pub(crate) fn find_matching_suffix(byte_seq: &[u8], buf: &[u8]) -> AlreadyFoundByteSeqCount {
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
    AlreadyFoundByteSeqCount(byte_seq_index)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn peek_some() {
        let bytes = b"123".as_ref();
        assert_eq!(peek(bytes), Some(b'1'));
    }

    #[test]
    fn peek_none() {
        let bytes = b"".as_ref();
        assert_eq!(peek(bytes), None);
    }

    #[test]
    fn peek2_some() {
        let bytes = b"12".as_ref();
        assert_eq!(peek2(bytes), Some(b'2'));

        let bytes = b"123".as_ref();
        assert_eq!(peek2(bytes), Some(b'2'));
    }

    #[test]
    fn peek2_none() {
        let bytes = b"".as_ref();
        assert_eq!(peek2(bytes), None);

        let bytes = b"1".as_ref();
        assert_eq!(peek2(bytes), None);
    }

    #[test]
    fn quote_aware_advanced_including_empty() {
        let bytes = r#""#.as_bytes();
        let found =
            quote_context_aware_find(bytes, QuoteState::None);
        assert_eq!(
            found,
            QuoteContextAwareFoundState::NotFound(QuoteState::None)
        );
    }

    #[test]
    fn quote_aware_advanced_including_found_with_no_quotes() {
        let bytes = r#"<?hello ?>"#.as_bytes();
        let found =
            quote_context_aware_find(bytes, QuoteState::None);
        assert_eq!(found, QuoteContextAwareFoundState::Found(10));
    }

    #[test]
    fn quote_aware_advanced_including_not_found_with_no_quotes() {
        let bytes = r#"<?hello "#.as_bytes();
        let found =
            quote_context_aware_find(bytes, QuoteState::None);
        assert_eq!(
            found,
            QuoteContextAwareFoundState::NotFound(QuoteState::None)
        );
    }

    #[test]
    fn quote_aware_advanced_including_end_unfinished_quote() {
        let bytes = r#"<?hello attr1="?>"#.as_bytes();
        let found =
            quote_context_aware_find(bytes, QuoteState::None);
        assert_eq!(
            found,
            QuoteContextAwareFoundState::NotFound(QuoteState::Double)
        );

        let bytes = r#""?>"#.as_bytes();
        let found = quote_context_aware_find(
            bytes,
            QuoteState::Double,
        );
        assert_eq!(found, QuoteContextAwareFoundState::Found(3));
    }

    #[test]
    fn quote_aware_advanced_including_start_quote_last_byte() {
        let bytes = r#"<?hello attr1=""#.as_bytes();
        let found =
            quote_context_aware_find(bytes, QuoteState::None);
        assert_eq!(
            found,
            QuoteContextAwareFoundState::NotFound(QuoteState::Double)
        );

        let bytes = r#"?>"?>"#.as_bytes();
        let found = quote_context_aware_find(
            bytes,
            QuoteState::Double,
        );
        assert_eq!(found, QuoteContextAwareFoundState::Found(5));
    }

    #[test]
    fn quote_aware_advanced_including_end_found_last_seq_byte_in_quote() {
        let bytes = r#"<?hello attr1=">"#.as_bytes();
        let found =
            quote_context_aware_find(bytes, QuoteState::None);
        assert_eq!(
            found,
            QuoteContextAwareFoundState::NotFound(QuoteState::Double)
        );

        let bytes = r#"?>"?>"#.as_bytes();
        let found = quote_context_aware_find(
            bytes,
            QuoteState::Double,
        );
        assert_eq!(found, QuoteContextAwareFoundState::Found(5));
    }

    #[test]
    fn find_matching_suffix_match_none() {
        let byte_seq = b"abc";
        let buf = b"d";
        assert_eq!(
            super::find_matching_suffix(byte_seq, buf),
            AlreadyFoundByteSeqCount(0)
        );

        let byte_seq = b"abc";
        let buf = b"de";
        assert_eq!(
            super::find_matching_suffix(byte_seq, buf),
            AlreadyFoundByteSeqCount(0)
        );

        let byte_seq = b"abc";
        let buf = b"def";
        assert_eq!(
            super::find_matching_suffix(byte_seq, buf),
            AlreadyFoundByteSeqCount(0)
        );

        let byte_seq = b"abc";
        let buf = b"defg";
        assert_eq!(
            super::find_matching_suffix(byte_seq, buf),
            AlreadyFoundByteSeqCount(0)
        );
    }

    #[test]
    fn find_matching_suffix_match_one() {
        let byte_seq = b"abc";
        let buf = b"a";
        assert_eq!(
            super::find_matching_suffix(byte_seq, buf),
            AlreadyFoundByteSeqCount(1)
        );

        let byte_seq = b"abc";
        let buf = b"xa";
        assert_eq!(
            super::find_matching_suffix(byte_seq, buf),
            AlreadyFoundByteSeqCount(1)
        );

        let byte_seq = b"abc";
        let buf = b"xya";
        assert_eq!(
            super::find_matching_suffix(byte_seq, buf),
            AlreadyFoundByteSeqCount(1)
        );

        let byte_seq = b"abc";
        let buf = b"xyza";
        assert_eq!(
            super::find_matching_suffix(byte_seq, buf),
            AlreadyFoundByteSeqCount(1)
        );
    }

    #[test]
    fn find_matching_suffix_match_multiple() {
        let byte_seq = b"abc";
        let buf = b"ab";
        assert_eq!(
            super::find_matching_suffix(byte_seq, buf),
            AlreadyFoundByteSeqCount(2)
        );

        let byte_seq = b"abc";
        let buf = b"xab";
        assert_eq!(
            super::find_matching_suffix(byte_seq, buf),
            AlreadyFoundByteSeqCount(2)
        );

        let byte_seq = b"abc";
        let buf = b"xyab";
        assert_eq!(
            super::find_matching_suffix(byte_seq, buf),
            AlreadyFoundByteSeqCount(2)
        );

        let byte_seq = b"abc";
        let buf = b"xyzab";
        assert_eq!(
            super::find_matching_suffix(byte_seq, buf),
            AlreadyFoundByteSeqCount(2)
        );
    }
}
