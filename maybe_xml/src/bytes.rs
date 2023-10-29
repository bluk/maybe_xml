//! An internal byte slice reader.

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub(crate) enum QuoteState {
    None,
    Single,
    Double,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub(crate) struct BracketCount(pub(crate) u64);

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub(crate) struct AlreadyFoundByteSeqCount(pub(crate) usize);

#[inline]
#[must_use]
pub(crate) fn peek(bytes: &[u8]) -> Option<u8> {
    bytes.first().copied()
}

#[inline]
#[must_use]
pub(crate) fn peek2(bytes: &[u8]) -> Option<u8> {
    let pos = 1;
    if pos < bytes.len() {
        Some(bytes[pos])
    } else {
        None
    }
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
}
