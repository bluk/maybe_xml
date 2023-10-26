//! Lexer for byte slice.

use core::iter;

use crate::token::Token;

mod scanner;

pub use scanner::scan;

/// Tokenizes XML input into a [`Token`].
///
/// The lexer can be used with complete or incremental input.
///
/// It does not allocate.
///
/// If the entire input is available, it may be easier to turn the `Lexer` into
/// an `Iterator` by calling either [`IntoIterator::into_iter()`] or
/// [`Lexer::iter()`][Lexer::iter()] on the lexer.
///
/// # Examples
///
/// ## Using [`Iterator`] functionality
///
/// ```
/// use maybe_xml::{Lexer, token::{Characters, EndTag, StartTag, Ty}};
///
/// let input = "<id>123</id><name>Jane Doe</name>";
///
/// let lexer = Lexer::from_str(input);
/// let mut iter = lexer.into_iter().filter_map(|token| {
///     match token.ty() {
///         Ty::StartTag(tag) => Some(tag.name().to_str()),
///         _ => None,
///     }
/// });
///
/// let name = iter.next().unwrap();
/// assert_eq!(Ok("id"), name);
///
/// let name = iter.next().unwrap();
/// assert_eq!(Ok("name"), name);
/// ```
///
/// ## Using [`Lexer::tokenize()`][Lexer::tokenize()] directly
///
/// ```
/// use maybe_xml::{Lexer, token::{Characters, EndTag, StartTag, Ty}};
///
/// let mut buf = Vec::new();
/// // Note the missing closing tag character `>` in the end tag.
/// buf.extend(b"<id>123</id");
///
/// let lexer = unsafe { Lexer::from_slice_unchecked(&buf) };
/// let mut pos = 0;
///
/// let token = lexer.tokenize(&mut pos).unwrap();
/// assert_eq!(0, token.offset());
/// assert_eq!(Ty::StartTag(StartTag::from("<id>".as_bytes())), token.ty());
///
/// // Position was assigned to the index after the end of the token
/// assert_eq!(4, pos);
///
/// let token = lexer.tokenize(&mut pos).unwrap();
/// assert_eq!(4, token.offset());
/// assert_eq!(Ty::Characters(Characters::from("123".as_bytes())), token.ty());
///
/// // Position was assigned to the index after the end of the token
/// assert_eq!(7, pos);
///
/// let token = lexer.tokenize(&mut pos);
/// // The last token is incomplete because it is missing the `>`
/// assert_eq!(None, token);
///
/// // Discard the tokenized input
/// buf.drain(..pos);
/// pos = 0;
///
/// // Wait for additional input
/// buf.extend(b">");
///
/// // Start tokenizing again with the input
/// let lexer = unsafe { Lexer::from_slice_unchecked(&buf) };
///
/// let token = lexer.tokenize(&mut pos).unwrap();
/// assert_eq!(0, token.offset());
/// assert_eq!(Ty::EndTag(EndTag::from("</id>".as_bytes())), token.ty());
///
/// // Position was assigned to the index after the end of the token
/// assert_eq!(5, pos);
///
/// let token = lexer.tokenize(&mut pos);
/// // There is no additional data to process
/// assert_eq!(None, token);
///
/// buf.drain(..pos);
/// pos = 0;
///
/// // End of file is reached while reading input
///
/// // Verify that the buffer is empty. If it is not empty, then there is data
/// // which could not be identified as a complete token. This usually indicates
/// // an error has occurred.
/// assert!(buf.is_empty());
/// ```
#[derive(Debug, Clone, Copy)]
pub struct Lexer<'a> {
    input: &'a [u8],
}

impl<'a> Lexer<'a> {
    /// Creates a new instance from a byte slice.
    ///
    /// # Safety
    ///
    /// The bytes are assumed to represent a valid UTF-8 string. If the bytes
    /// are not UTF-8, then any methods called on this type are undefined.
    #[inline]
    #[must_use]
    pub const unsafe fn from_slice_unchecked(input: &'a [u8]) -> Self {
        Self { input }
    }

    /// Creates a new instance with the given UTF-8 string input.
    #[inline]
    #[must_use]
    pub const fn from_str(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
        }
    }

    /// Creates a new instance with the given UTF-8 string input.
    #[inline]
    #[must_use]
    pub const fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
        }
    }

    /// Tokenizes the input starting at the given position.
    ///
    /// If a token is found, the position is also updated to after the token.
    ///
    /// # Examples
    ///
    /// ```
    /// use maybe_xml::{Lexer, token::{Characters, EndTag, StartTag, Ty}};
    ///
    /// let input = "<id>123</id>";
    ///
    /// let lexer = Lexer::from_str(input);
    /// let mut pos = 0;
    ///
    /// let token = lexer.tokenize(&mut pos).unwrap();
    /// assert_eq!(0, token.offset());
    /// assert_eq!(Ty::StartTag(StartTag::from("<id>".as_bytes())), token.ty());
    ///
    /// // Position was assigned to the index after the end of the token
    /// assert_eq!(4, pos);
    ///```
    #[must_use]
    pub fn tokenize(&self, pos: &mut usize) -> Option<Token<'a>> {
        let bytes = &self.input[*pos..];
        let ty = scan(bytes)?;
        let token = Token::new(ty, *pos);
        *pos += token.len();
        Some(token)
    }

    /// Returns an iterator for tokens starting at the given position.
    ///
    /// The `pos` parameter is **not** updated.
    ///
    /// # Examples
    ///
    /// ## Using other [`Iterator`] functionality
    ///
    /// ```
    /// use maybe_xml::{Lexer, token::{Characters, EndTag, StartTag, Ty}};
    ///
    /// let input = "<id>123</id><name>Jane Doe</name>";
    ///
    /// let lexer = Lexer::from_str(input);
    /// let mut iter = lexer.iter(0).filter_map(|token| {
    ///     match token.ty() {
    ///         Ty::StartTag(tag) => Some(tag.name().to_str()),
    ///         _ => None,
    ///     }
    /// });
    ///
    /// let name = iter.next().unwrap();
    /// assert_eq!(Ok("id"), name);
    ///
    /// let name = iter.next().unwrap();
    /// assert_eq!(Ok("name"), name);
    /// ```
    ///
    /// ## Considerations during iteration
    ///
    /// ```
    /// use maybe_xml::{Lexer, token::{Characters, EndTag, StartTag, Ty}};
    ///
    /// let mut buf = Vec::new();
    /// // Note the missing closing tag character `>` in the end tag.
    /// buf.extend(b"Test<id>123</id");
    ///
    /// let lexer = unsafe { Lexer::from_slice_unchecked(&buf) };
    ///
    /// // Start after the initial text content
    /// let pos = 4;
    ///
    /// let mut iter = lexer.iter(pos);
    ///
    /// let token = iter.next().unwrap();
    /// assert_eq!(4, token.offset());
    /// assert_eq!(Ty::StartTag(StartTag::from("<id>".as_bytes())), token.ty());
    ///
    /// let pos = token.offset() + token.len();
    ///
    /// let token = iter.next().unwrap();
    /// assert_eq!(8, token.offset());
    /// assert_eq!(Ty::Characters(Characters::from("123".as_bytes())), token.ty());
    ///
    /// let pos = token.offset() + token.len();
    ///
    /// let token = iter.next();
    /// // The last token is incomplete because it is missing the `>`
    /// assert_eq!(None, token);
    ///
    /// drop(iter);
    ///
    /// // Discard the tokenized input
    /// buf.drain(..pos);
    ///
    /// let pos = 0;
    ///
    /// // Wait for additional input
    /// buf.extend(b">");
    ///
    /// // Start tokenizing again with the input
    /// let lexer = unsafe { Lexer::from_slice_unchecked(&buf) };
    ///
    /// let mut iter = lexer.iter(pos);
    ///
    /// let token = iter.next().unwrap();
    /// assert_eq!(0, token.offset());
    /// assert_eq!(Ty::EndTag(EndTag::from("</id>".as_bytes())), token.ty());
    ///
    /// let pos = token.offset() + token.len();
    ///
    /// // Position was assigned to the index after the end of the token
    /// assert_eq!(5, pos);
    ///
    /// let token = iter.next();
    /// // There is no additional data to process
    /// assert_eq!(None, token);
    ///
    /// drop(iter);
    ///
    /// buf.drain(..pos);
    ///
    /// let pos = 0;
    ///
    /// // End of file is reached while reading input
    ///
    /// // Verify that the buffer is empty. If it is not empty, then there is data
    /// // which could not be identified as a complete token. This usually indicates
    /// // an error has occurred.
    /// assert!(buf.is_empty());
    /// ```
    #[inline]
    pub fn iter(&self, mut pos: usize) -> impl Iterator<Item = Token<'a>> + '_ {
        iter::from_fn(move || self.tokenize(&mut pos))
    }
}

impl<'a> IntoIterator for Lexer<'a> {
    type Item = Token<'a>;

    type IntoIter = IntoIter<'a>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        IntoIter::new(self, 0)
    }
}

/// The returned iterator type when [`IntoIterator::into_iter()`] is called on [`Lexer`].
///
/// # Example
///
/// ```
/// use maybe_xml::{Lexer, token::{EndTag, StartTag, Ty}};
/// use std::io::{BufRead, BufReader};
///
/// let input = "<ID>Example</id><name>Jane Doe</name>";
/// let lexer = Lexer::new(input);
///
/// let mut iter = lexer.into_iter()
///     .filter_map(|token| {
///         match token.ty() {
///             Ty::StartTag(start_tag) => {
///                 Some(start_tag.name().to_str())
///             }
///             Ty::EndTag(end_tag) => {
///                 Some(end_tag.name().to_str())
///             }
///             _ => None,
///         }
///     });
///
/// assert_eq!(Some(Ok("ID")), iter.next());
/// assert_eq!(Some(Ok("id")), iter.next());
/// assert_eq!(Some(Ok("name")), iter.next());
/// assert_eq!(Some(Ok("name")), iter.next());
/// assert_eq!(None, iter.next());
/// # Ok::<(), std::io::Error>(())
/// ```
#[derive(Debug)]
pub struct IntoIter<'a> {
    inner: Lexer<'a>,
    pos: usize,
}

impl<'a> IntoIter<'a> {
    #[inline]
    #[must_use]
    pub const fn new(inner: Lexer<'a>, pos: usize) -> Self {
        Self { inner, pos }
    }
}

impl<'a> Iterator for IntoIter<'a> {
    type Item = Token<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.tokenize(&mut self.pos)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(all(feature = "alloc", not(feature = "std")))]
    use alloc::vec::Vec;
    #[cfg(feature = "std")]
    use std::vec::Vec;

    #[cfg(any(feature = "std", feature = "alloc"))]
    use crate::token::Ty;

    #[test]
    fn none_on_empty() {
        let lexer = Lexer::from_str("");
        let mut pos = 0;
        assert_eq!(None, lexer.tokenize(&mut pos));
    }

    #[test]
    #[should_panic(expected = "start index 1 out of range")]
    fn panic_on_pos_greater_than_slice_len() {
        let lexer = Lexer::from_str("");
        let mut pos = 1;
        let _ = lexer.tokenize(&mut pos);
    }

    #[test]
    #[should_panic(expected = "out of range")]
    fn panic_on_pos_greater_than_slice_len_2() {
        let lexer = Lexer::from_str("hello");
        let mut pos = "hello".len() + 1;
        let _ = lexer.tokenize(&mut pos);
    }

    #[cfg(any(feature = "std", feature = "alloc"))]
    #[test]
    fn text_content() {
        use crate::token::Characters;

        let mut buf = Vec::new();
        let mut pos = 0;
        buf.extend("Hello".as_bytes());
        let lexer = unsafe { Lexer::from_slice_unchecked(&buf) };
        assert_eq!(
            Some(Token::new(
                Ty::Characters(Characters::from("Hello".as_bytes())),
                0
            )),
            lexer.tokenize(&mut pos)
        );
        assert_eq!(buf.len(), pos);

        buf.extend("wo".as_bytes());
        let lexer = unsafe { Lexer::from_slice_unchecked(&buf) };
        assert_eq!(
            Some(Token::new(
                Ty::Characters(Characters::from("wo".as_bytes())),
                5,
            )),
            lexer.tokenize(&mut pos)
        );
        assert_eq!(buf.len(), pos);

        buf.extend("rld!<".as_bytes());
        let lexer = unsafe { Lexer::from_slice_unchecked(&buf) };
        assert_eq!(
            Some(Token::new(
                Ty::Characters(Characters::from("rld!".as_bytes())),
                7,
            )),
            lexer.tokenize(&mut pos)
        );
        assert_eq!(buf.len() - 1, pos);
    }
}
