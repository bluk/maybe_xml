//! Reader for `&str`.

use crate::token::Token;

mod scanner;

use scanner::scan;

/// Tokenizes XML input into a [`Token`].
///
/// It does not allocate.
///
/// # Examples
///
/// ## Using [`tokenize()`][Reader::tokenize()]
///
/// ```
/// use maybe_xml::{Reader, token::{Characters, EndTag, StartTag, Ty}};
///
/// let input = "<id>123</id>";
///
/// let reader = Reader::from_str(input);
/// let mut pos = 0;
///
/// let token = reader.tokenize(&mut pos);
/// if let Some(Ty::StartTag(tag)) = token.map(|t| t.ty()) {
///     assert_eq!("id", tag.name().local().as_str());
///     assert_eq!(None, tag.name().namespace_prefix());
/// } else {
///     panic!();
/// }
/// assert_eq!(4, pos);
///
/// let token = reader.tokenize(&mut pos);
/// if let Some(Ty::Characters(chars)) = token.map(|t| t.ty()) {
///     assert_eq!("123", chars.content().as_str());
/// } else {
///     panic!();
/// }
/// assert_eq!(7, pos);
///
/// let token = reader.tokenize(&mut pos);
/// if let Some(Ty::EndTag(tag)) = token.map(|t| t.ty()) {
///     assert_eq!("</id>", tag.as_str());
///     assert_eq!("id", tag.name().local().as_str());
/// } else {
///     panic!();
/// }
/// assert_eq!(12, pos);
///
/// let token = reader.tokenize(&mut pos);
/// assert_eq!(None, token);
///
/// // Verify that `pos` is equal to `input.len()` to ensure all data was
/// // processed.
/// ```
///
/// ## Using [`Iterator`] functionality
///
/// ```
/// use maybe_xml::{Reader, token::{Characters, EndTag, StartTag, Ty}};
///
/// let input = "<id>123</id><name>Jane Doe</name>";
///
/// let reader = Reader::from_str(input);
/// let mut iter = reader.into_iter().filter_map(|token| {
///     match token.ty() {
///         Ty::StartTag(tag) => Some(tag.name().as_str()),
///         _ => None,
///     }
/// });
///
/// let name = iter.next();
/// assert_eq!(Some("id"), name);
///
/// let name = iter.next();
/// assert_eq!(Some("name"), name);
///
/// assert_eq!(None, iter.next());
/// ```
///
/// Note that if the input is malformed or incomplete such as `<tag`, the
/// Iterator will return `None` and will not return the invalid input. If you
/// want to verify that all of the input was processed, then you should use the
/// [`Reader::tokenize()`][Reader::tokenize()] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Reader<'a> {
    input: &'a str,
}

impl<'a> Reader<'a> {
    /// Creates a new instance from a byte slice.
    ///
    /// # Safety
    ///
    /// The bytes are assumed to represent a valid UTF-8 string. If the bytes
    /// are not UTF-8, then any methods called on this type are undefined.
    ///
    /// # Example
    ///
    /// ```
    /// use maybe_xml::{Reader, token::{Characters, EndTag, StartTag, Ty}};
    ///
    /// let mut buf = Vec::new();
    /// // Note the missing closing tag character `>` in the end tag.
    /// buf.extend(b"<id>123</id");
    ///
    /// let reader = unsafe { Reader::from_slice_unchecked(&buf) };
    /// let mut pos = 0;
    ///
    /// let token = reader.tokenize(&mut pos);
    /// if let Some(Ty::StartTag(tag)) = token.map(|t| t.ty()) {
    ///     assert_eq!("id", tag.name().local().as_str());
    ///     assert_eq!(None, tag.name().namespace_prefix());
    /// } else {
    ///     panic!();
    /// }
    ///
    /// // Position was assigned to the index after the end of the token
    /// assert_eq!(4, pos);
    ///
    /// let token = reader.tokenize(&mut pos);
    /// if let Some(Ty::Characters(chars)) = token.map(|t| t.ty()) {
    ///     assert_eq!("123", chars.content().as_str());
    /// } else {
    ///     panic!();
    /// }
    ///
    /// // Position was assigned to the index after the end of the token
    /// assert_eq!(7, pos);
    ///
    /// let token = reader.tokenize(&mut pos);
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
    /// let reader = unsafe { Reader::from_slice_unchecked(&buf) };
    ///
    /// let token = reader.tokenize(&mut pos);
    /// if let Some(Ty::EndTag(tag)) = token.map(|t| t.ty()) {
    ///     assert_eq!("</id>", tag.as_str());
    ///     assert_eq!("id", tag.name().local().as_str());
    /// } else {
    ///     panic!();
    /// }
    ///
    /// // Position was assigned to the index after the end of the token
    /// assert_eq!(5, pos);
    ///
    /// let token = reader.tokenize(&mut pos);
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
    #[deprecated(
        since = "0.9.0",
        note = "Use core::str::from_utf8_unchecked() and then use Reader::from_str() instead."
    )]
    #[inline]
    #[must_use]
    pub const unsafe fn from_slice_unchecked(input: &'a [u8]) -> Self {
        Self {
            input: core::str::from_utf8_unchecked(input),
        }
    }

    /// Creates a new instance with the given UTF-8 string input.
    #[inline]
    #[must_use]
    pub const fn from_str(input: &'a str) -> Self {
        Self { input }
    }

    /// Creates a new instance with the given UTF-8 string input.
    #[inline]
    #[must_use]
    pub const fn new(input: &'a str) -> Self {
        Self { input }
    }

    /// Tokenizes the input starting at the given position.
    ///
    /// If a token is found, the position argument is also updated to the byte
    /// index after the token.
    ///
    /// # Panics
    ///
    /// Panics if the `pos` is greater than the input length or if `pos` is
    /// not at a character boundary.
    ///
    /// # Examples
    ///
    /// ```
    /// use maybe_xml::{Reader, token::{StartTag, Ty}};
    ///
    /// let input = "<id>123</id>";
    ///
    /// let reader = Reader::from_str(input);
    /// let mut pos = 0;
    ///
    /// let token = reader.tokenize(&mut pos);
    /// if let Some(Ty::StartTag(tag)) = token.map(|t| t.ty()) {
    ///     assert_eq!("id", tag.name().local().as_str());
    ///     assert_eq!(None, tag.name().namespace_prefix());
    /// } else {
    ///     panic!();
    /// }
    ///
    /// // Position was assigned to the index after the end of the token
    /// assert_eq!(4, pos);
    /// ```
    ///
    /// If `tokenize()` returns `None`, but the position is not equal to the input's
    /// byte length, then there is unprocessed input such as malformed XML. For
    /// instance, if the input was `<tag` without the enclosing `>`, then
    /// `tokenize()` will return `None`.
    ///
    /// ```
    /// use maybe_xml::{Reader, token::{StartTag, Ty}};
    ///
    /// let input = "<tag";
    ///
    /// let reader = Reader::from_str(input);
    /// let mut pos = 0;
    ///
    /// let token = reader.tokenize(&mut pos);
    /// assert_eq!(None, token);
    ///
    /// assert_eq!(0, pos);
    /// assert_ne!(input.len(), pos);
    /// ```
    #[must_use]
    pub fn tokenize(&self, pos: &mut usize) -> Option<Token<'a>> {
        let input = self.input.as_bytes();
        if input.len() == *pos {
            return None;
        }

        assert!(crate::is_utf8_boundary(input[*pos]));

        let end = scan(input, *pos)?;
        let token = Token::from_str(unsafe { core::str::from_utf8_unchecked(&input[*pos..end]) });
        *pos = end;
        Some(token)
    }

    /// Constant function which tokenizes the input starting at the given position.
    ///
    /// # Important
    ///
    /// The `pos` is **not** updated and should be updated with the
    /// [`Token::len()`][Token::len()].
    ///
    /// # Panics
    ///
    /// Panics if the `pos` is greater than the input length or if `pos` is
    /// not at a character boundary.
    ///
    /// # Examples
    ///
    /// ```
    /// use maybe_xml::{Reader, token::{StartTag, Ty}};
    ///
    /// let input = "<id>123</id>";
    ///
    /// let reader = Reader::from_str(input);
    /// let mut pos = 0;
    ///
    /// let token = reader.parse(pos);
    /// if let Some(Ty::StartTag(tag)) = token.map(|t| t.ty()) {
    ///     assert_eq!("id", tag.name().local().as_str());
    ///     assert_eq!(None, tag.name().namespace_prefix());
    /// } else {
    ///     panic!();
    /// }
    ///
    /// pos += token.map(|t| t.len()).unwrap_or_default();
    /// assert_eq!(4, pos);
    /// ```
    ///
    /// If `parse()` returns `None`, but the position is not equal to the input's
    /// byte length, then there is unprocessed input such as malformed XML. For
    /// instance, if the input was `<tag` without the enclosing `>`, then
    /// `tokenize()` will return `None`.
    ///
    /// ```
    /// use maybe_xml::{Reader, token::{StartTag, Ty}};
    ///
    /// let input = "<tag";
    ///
    /// let reader = Reader::from_str(input);
    /// let mut pos = 0;
    ///
    /// let token = reader.parse(pos);
    /// assert_eq!(None, token);
    ///
    /// assert_eq!(0, pos);
    /// assert_ne!(input.len(), pos);
    /// ```
    #[rustversion::attr(since(1.71), const)]
    #[must_use]
    pub fn parse(&self, pos: usize) -> Option<Token<'a>> {
        let input = self.input.as_bytes();

        if input.len() == pos {
            return None;
        }

        assert!(
            crate::is_utf8_boundary(input[pos]),
            "pos is not at a character boundary"
        );

        if let Some(end) = scan(input, pos) {
            // This is a convoluted but *const* way of getting &self.input[*pos..end]
            let (bytes, _) = input.split_at(end);
            let (_, bytes) = bytes.split_at(pos);
            let token = Token::from_str(unsafe { core::str::from_utf8_unchecked(bytes) });
            Some(token)
        } else {
            None
        }
    }

    /// Returns an iterator for tokens starting at the given position.
    ///
    /// # Panics
    ///
    /// The iterator will panic if the initial `pos` is greater than the input
    /// length or if `pos` is not at a character boundary.
    ///
    /// # Examples
    ///
    /// ## Using other [`Iterator`] functionality
    ///
    /// ```
    /// use maybe_xml::{Reader, token::Ty};
    ///
    /// let input = "<id>123</id><name>Jane Doe</name>";
    ///
    /// let reader = Reader::from_str(input);
    /// let mut iter = reader.iter(0).filter_map(|token| {
    ///     match token.ty() {
    ///         Ty::StartTag(tag) => Some(tag.name().as_str()),
    ///         _ => None,
    ///     }
    /// });
    ///
    /// let name = iter.next();
    /// assert_eq!(Some("id"), name);
    ///
    /// let name = iter.next();
    /// assert_eq!(Some("name"), name);
    ///
    /// assert_eq!(None, iter.next());
    /// ```
    #[inline]
    #[must_use]
    pub const fn iter(&self, pos: usize) -> Iter<'a> {
        Iter::new(*self, pos)
    }

    /// Return the underlying bytes being tokenized.
    #[inline]
    #[must_use]
    pub const fn into_inner(self) -> &'a str {
        self.input
    }
}

impl<'a> IntoIterator for Reader<'a> {
    type Item = Token<'a>;

    type IntoIter = IntoIter<'a>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        IntoIter::new(self, 0)
    }
}

/// The returned iterator type for [`Reader::iter()`].
///
/// # Example
///
/// ```
/// use maybe_xml::{Reader, token::Ty};
///
/// let input = "<ID>Example</id><name>Jane Doe</name>";
/// let reader = Reader::new(input);
///
/// let mut iter = reader.iter(4)
///     .filter_map(|token| {
///         match token.ty() {
///             Ty::StartTag(start_tag) => {
///                 Some(start_tag.name().as_str())
///             }
///             Ty::EndTag(end_tag) => {
///                 Some(end_tag.name().as_str())
///             }
///             _ => None,
///         }
///     });
///
/// assert_eq!(Some("id"), iter.next());
/// assert_eq!(Some("name"), iter.next());
/// assert_eq!(Some("name"), iter.next());
/// assert_eq!(None, iter.next());
/// ```
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Iter<'a> {
    inner: Reader<'a>,
    pos: usize,
}

impl<'a> Iter<'a> {
    #[inline]
    #[must_use]
    const fn new(inner: Reader<'a>, pos: usize) -> Self {
        Self { inner, pos }
    }
}

impl<'a> Iterator for Iter<'a> {
    type Item = Token<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.tokenize(&mut self.pos)
    }
}

/// The returned iterator type when [`IntoIterator::into_iter()`] is called on [`Reader`].
///
/// # Example
///
/// ```
/// use maybe_xml::{Reader, token::Ty};
///
/// let input = "<ID>Example</id><name>Jane Doe</name>";
/// let reader = Reader::new(input);
///
/// let mut iter = reader.into_iter()
///     .filter_map(|token| {
///         match token.ty() {
///             Ty::StartTag(start_tag) => {
///                 Some(start_tag.name().as_str())
///             }
///             Ty::EndTag(end_tag) => {
///                 Some(end_tag.name().as_str())
///             }
///             _ => None,
///         }
///     });
///
/// assert_eq!(Some("ID"), iter.next());
/// assert_eq!(Some("id"), iter.next());
/// assert_eq!(Some("name"), iter.next());
/// assert_eq!(Some("name"), iter.next());
/// assert_eq!(None, iter.next());
/// ```
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct IntoIter<'a> {
    inner: Reader<'a>,
    pos: usize,
}

impl<'a> IntoIter<'a> {
    #[inline]
    #[must_use]
    const fn new(inner: Reader<'a>, pos: usize) -> Self {
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
    use crate::token::{
        Cdata, Characters, Comment, Declaration, EmptyElementTag, EndTag, ProcessingInstruction,
        StartTag, Ty,
    };

    use super::*;

    #[cfg(all(feature = "alloc", not(feature = "std")))]
    extern crate alloc;

    #[cfg(all(feature = "alloc", not(feature = "std")))]
    use alloc::vec::Vec;
    #[cfg(feature = "std")]
    use std::vec::Vec;

    #[test]
    fn none_on_empty() {
        let reader = Reader::from_str("");
        let mut pos = 0;
        assert_eq!(None, reader.tokenize(&mut pos));
        assert_eq!(0, pos);
    }

    #[test]
    #[should_panic(expected = "out of bounds")]
    fn panic_on_pos_greater_than_slice_len() {
        let reader = Reader::from_str("");
        let mut pos = 1;
        let _ = reader.tokenize(&mut pos);
    }

    #[test]
    #[should_panic(expected = "out of bounds")]
    fn panic_on_pos_greater_than_slice_len_2() {
        let reader = Reader::from_str("hello");
        let mut pos = "hello".len() + 1;
        let _ = reader.tokenize(&mut pos);
    }

    #[test]
    #[should_panic(expected = "pos")]
    fn test_utf8() {
        let input = "你好";

        let reader = Reader::from_str(input);
        let mut pos = 1;
        let _ = reader.tokenize(&mut pos);
    }

    #[cfg(any(feature = "std", feature = "alloc"))]
    #[test]
    fn text_content() {
        let mut buf = Vec::new();
        let mut pos = 0;
        buf.extend("Hello".as_bytes());
        let reader = unsafe { Reader::from_slice_unchecked(&buf) };
        assert_eq!(Some("Hello"), reader.tokenize(&mut pos).map(|t| t.as_str()));
        assert_eq!(buf.len(), pos);

        buf.extend("wo".as_bytes());
        let reader = unsafe { Reader::from_slice_unchecked(&buf) };
        assert_eq!(Some("wo"), reader.tokenize(&mut pos).map(|t| t.as_str()));
        assert_eq!(buf.len(), pos);

        buf.extend("rld!<".as_bytes());
        let reader = unsafe { Reader::from_slice_unchecked(&buf) };
        assert_eq!(Some("rld!"), reader.tokenize(&mut pos).map(|t| t.as_str()));
        assert_eq!(buf.len() - 1, pos);
    }

    fn verify_tokenize_all(input: &str, expected: &[Ty<'_>]) {
        verify_tokenize(input, 0, expected, input.len());
    }

    fn verify_tokenize(input: &str, mut pos: usize, expected: &[Ty<'_>], end: usize) {
        let reader = Reader::from_str(input);

        for e in expected.iter().copied() {
            assert_eq!(Some(e), reader.tokenize(&mut pos).map(|token| token.ty()));
        }

        assert_eq!(None, reader.tokenize(&mut pos));
        assert_eq!(pos, end);
    }

    #[test]
    fn characters() {
        verify_tokenize_all("Hello", &[Ty::Characters(Characters::from_str("Hello"))]);
        verify_tokenize_all(" wo", &[Ty::Characters(Characters::from_str(" wo"))]);
        verify_tokenize(
            "rld!<",
            0,
            &[Ty::Characters(Characters::from_str("rld!"))],
            4,
        );
    }

    #[test]
    fn incomplete_start_of_markup() {
        verify_tokenize("<", 0, &[], 0);
    }

    #[test]
    fn start_tag() {
        let input = "<hello>";
        verify_tokenize_all(input, &[Ty::StartTag(StartTag::from_str(input))]);
    }

    #[test]
    fn start_tag_with_more_at_end() {
        let input = "<hello>Content";
        verify_tokenize_all(
            input,
            &[
                Ty::StartTag(StartTag::from_str("<hello>")),
                Ty::Characters(Characters::from_str("Content")),
            ],
        );
    }

    #[test]
    fn start_tag_with_single_quotes_attribute() {
        let input = "<hello a='val>'>Content";
        verify_tokenize_all(
            input,
            &[
                Ty::StartTag(StartTag::from_str("<hello a='val>'>")),
                Ty::Characters(Characters::from_str("Content")),
            ],
        );
    }

    #[test]
    fn start_tag_with_double_quotes_attribute() {
        let input = r#"<hello a="val>">"#;
        verify_tokenize_all(input, &[Ty::StartTag(StartTag::from_str(input))]);
    }

    #[test]
    fn empty_element_tag() {
        let input = "<hello/>";
        verify_tokenize_all(
            input,
            &[Ty::EmptyElementTag(EmptyElementTag::from_str(input))],
        );
    }

    #[test]
    fn empty_element_tag_space_after_slash_means_start_tag() {
        let input = "<hello / >";
        verify_tokenize_all(input, &[Ty::StartTag(StartTag::from_str(input))]);
    }

    #[test]
    fn empty_element_tag_with_double_quotes_attribute() {
        let input = r#"<hello a="val/>"/>"#;
        verify_tokenize_all(
            input,
            &[Ty::EmptyElementTag(EmptyElementTag::from_str(input))],
        );
    }

    #[test]
    fn empty_element_tag_with_last_slash_means_start_tag() {
        let input = "<hello/ invalid>";
        verify_tokenize_all(input, &[Ty::StartTag(StartTag::from_str(input))]);
    }

    #[test]
    fn end_tag() {
        let input = "</goodbye>";
        verify_tokenize_all(input, &[Ty::EndTag(EndTag::from_str(input))]);
    }

    #[test]
    fn end_tag_with_single_quotes_attribute() {
        let input = "</goodbye a='val>'>Content";
        verify_tokenize_all(
            input,
            &[
                Ty::EndTag(EndTag::from_str("</goodbye a='val>'>")),
                Ty::Characters(Characters::from_str("Content")),
            ],
        );
    }

    #[test]
    fn end_tag_with_double_quotes_attribute() {
        let input = r#"</goodbye a="val>">Content"#;
        verify_tokenize_all(
            input,
            &[
                Ty::EndTag(EndTag::from_str("</goodbye a=\"val>\">")),
                Ty::Characters(Characters::from_str("Content")),
            ],
        );
    }

    #[test]
    fn processing_instruction() {
        let input = r#"<?test a="b" ?>"#;
        verify_tokenize_all(
            input,
            &[Ty::ProcessingInstruction(ProcessingInstruction::from_str(
                input,
            ))],
        );
    }

    #[test]
    fn processing_instruction_with_broken_delimiter() {
        let input = r#"<?test? > a="v"?>"#;
        verify_tokenize_all(
            input,
            &[Ty::ProcessingInstruction(ProcessingInstruction::from_str(
                input,
            ))],
        );
    }

    #[test]
    fn pi_with_single_quotes_attribute() {
        let input = "<?goodbye a='val>'?>Content";
        verify_tokenize_all(
            input,
            &[
                Ty::ProcessingInstruction(ProcessingInstruction::from_str("<?goodbye a='val>'?>")),
                Ty::Characters(Characters::from_str("Content")),
            ],
        );
    }

    #[test]
    fn pi_with_double_quotes_attribute() {
        let input = r#"<?goodbye a="val>"?>Content"#;
        verify_tokenize_all(
            input,
            &[
                Ty::ProcessingInstruction(ProcessingInstruction::from_str(
                    "<?goodbye a=\"val>\"?>",
                )),
                Ty::Characters(Characters::from_str("Content")),
            ],
        );
    }

    #[test]
    fn pi_not_reuse_question_mark() {
        // The '>' byte here is treated as part of the tag's content because a "?>" is expected
        let input = "<?>";
        verify_tokenize(input, 0, &[], 0);
    }

    #[test]
    fn declaration_in_one_pass() {
        let input = "<!DOCTYPE test [<!ELEMENT test (#PCDATA)>]>";
        verify_tokenize_all(input, &[Ty::Declaration(Declaration::from_str(input))]);
    }

    #[test]
    fn declaration_with_single_quotes_attribute() {
        let input = "<!goodbye a='val>'>Content";
        verify_tokenize_all(
            input,
            &[
                Ty::Declaration(Declaration::from_str("<!goodbye a='val>'>")),
                Ty::Characters(Characters::from_str("Content")),
            ],
        );
    }

    #[test]
    fn declaration_with_double_quotes_attribute() {
        let input = r#"<!goodbye a="val>">Content"#;
        verify_tokenize_all(
            input,
            &[
                Ty::Declaration(Declaration::from_str("<!goodbye a=\"val>\">")),
                Ty::Characters(Characters::from_str("Content")),
            ],
        );
    }

    #[test]
    fn declaration_with_closed_brackets() {
        let input = "<![%test;[<!ELEMENT test (something*)>]]>";
        verify_tokenize_all(
            input,
            &[Ty::Declaration(Declaration::from_str(
                "<![%test;[<!ELEMENT test (something*)>]]>",
            ))],
        );
    }

    #[test]
    fn declaration_with_unclosed_single_bracket() {
        let input = "<![test>>] >Content";
        verify_tokenize_all(
            input,
            &[
                Ty::Declaration(Declaration::from_str("<![test>>] >")),
                Ty::Characters(Characters::from_str("Content")),
            ],
        );
    }

    #[test]
    fn declaration_with_unclosed_double_bracket() {
        let input = "<![test>[more>>] >Content>>] >Content";
        verify_tokenize_all(
            input,
            &[
                Ty::Declaration(Declaration::from_str("<![test>[more>>] >Content>>] >")),
                Ty::Characters(Characters::from_str("Content")),
            ],
        );
    }

    #[test]
    fn comment_in_one_pass() {
        let input = "<!-- Comment -->";
        verify_tokenize_all(input, &[Ty::Comment(Comment::from_str("<!-- Comment -->"))]);
    }

    #[test]
    fn comment_with_trailing_data() {
        let input = "<!-- Comment -->Content";
        verify_tokenize_all(
            input,
            &[
                Ty::Comment(Comment::from_str("<!-- Comment -->")),
                Ty::Characters(Characters::from_str("Content")),
            ],
        );
    }

    #[test]
    fn comment_with_single_quotes_attribute() {
        let input = "<!-- goodbye a='val-->Content";
        verify_tokenize_all(
            input,
            &[
                Ty::Comment(Comment::from_str("<!-- goodbye a='val-->")),
                Ty::Characters(Characters::from_str("Content")),
            ],
        );
    }

    #[test]
    fn comment_with_double_quotes_attribute() {
        let input = r#"<!-- goodbye a="val-->Content"#;
        verify_tokenize_all(
            input,
            &[
                Ty::Comment(Comment::from_str("<!-- goodbye a=\"val-->")),
                Ty::Characters(Characters::from_str("Content")),
            ],
        );
    }

    #[test]
    fn comment_with_invalid_start_means_declaration() {
        let input = r#"<!-goodbye a="-->val-->">Content"#;
        verify_tokenize_all(
            input,
            &[
                Ty::Declaration(Declaration::from_str("<!-goodbye a=\"-->val-->\">")),
                Ty::Characters(Characters::from_str("Content")),
            ],
        );
    }

    #[test]
    fn comment_with_double_dash_inside() {
        let input = r#"<!--goodbye a="--"#;
        verify_tokenize(input, 0, &[], 0);

        let input = r#"<!--goodbye a="--val-->"-- test -->Content"#;
        verify_tokenize_all(
            input,
            &[
                Ty::Comment(Comment::from_str("<!--goodbye a=\"--val-->")),
                Ty::Characters(Characters::from_str("\"-- test -->Content")),
            ],
        );
    }

    #[test]
    fn comment_with_single_dash() {
        let input = r#"<!--goodbye a="--"#;
        verify_tokenize(input, 0, &[], 0);

        let input = r#"<!--goodbye a="--val--" test ->Content"#;
        verify_tokenize(input, 0, &[], 0);

        let input = r#"<!--goodbye a="--val--" test ->ContentMore -->Real Content"#;
        verify_tokenize_all(
            input,
            &[
                Ty::Comment(Comment::from_str(
                    r#"<!--goodbye a="--val--" test ->ContentMore -->"#,
                )),
                Ty::Characters(Characters::from_str("Real Content")),
            ],
        );
    }

    #[test]
    fn comment_not_reused_dashes() {
        let input = "<!-->-->";
        verify_tokenize_all(input, &[Ty::Comment(Comment::from_str(input))]);
    }

    #[test]
    fn comment_not_reused_dashes_missing_close() {
        let input = "<!-->";
        verify_tokenize(input, 0, &[], 0);
    }

    #[test]
    fn cdata() {
        let input = "<![CDATA[ Content ]]>";
        verify_tokenize_all(input, &[Ty::Cdata(Cdata::from_str(input))]);
    }

    #[test]
    fn declaration_with_uneven_brackets() {
        let input = "<![&random[ Declaration ]]]>";
        verify_tokenize_all(input, &[Ty::Declaration(Declaration::from_str(input))]);
    }

    #[test]
    fn cdata_with_trailing_data() {
        let input = "<![CDATA[ Content ]]> Unused Content";
        verify_tokenize_all(
            input,
            &[
                Ty::Cdata(Cdata::from_str("<![CDATA[ Content ]]>")),
                Ty::Characters(Characters::from_str(" Unused Content")),
            ],
        );
    }

    #[test]
    fn cdata_with_no_space_trailing_data() {
        let input = "<![CDATA[ Content ]]>Content";
        verify_tokenize_all(
            input,
            &[
                Ty::Cdata(Cdata::from_str("<![CDATA[ Content ]]>")),
                Ty::Characters(Characters::from_str("Content")),
            ],
        );
    }

    #[test]
    fn cdata_with_single_quotes() {
        // The single quote does not escape here
        let input = "<![CDATA[ Content ']]>']]>Unused Content";
        verify_tokenize_all(
            input,
            &[
                Ty::Cdata(Cdata::from_str("<![CDATA[ Content ']]>")),
                Ty::Characters(Characters::from_str("']]>Unused Content")),
            ],
        );
    }

    #[test]
    fn cdata_with_double_quotes() {
        let input = r#"<![CDATA[ goodbye a="]]>"]]>Content"#;
        verify_tokenize_all(
            input,
            &[
                Ty::Cdata(Cdata::from_str(r#"<![CDATA[ goodbye a="]]>"#)),
                Ty::Characters(Characters::from_str("\"]]>Content")),
            ],
        );
    }

    #[test]
    fn cdata_with_invalid_start_means_declaration() {
        // missing "["
        let input = r#"<![CDATA Content a="]]>"#;
        verify_tokenize(input, 0, &[], 0);

        let input = r#"<![CDATA Content a="]]>]]>"]]>Content"#;
        verify_tokenize_all(
            input,
            &[
                Ty::Declaration(Declaration::from_str(r#"<![CDATA Content a="]]>]]>"]]>"#)),
                Ty::Characters(Characters::from_str("Content")),
            ],
        );
    }

    #[test]
    fn cdata_with_double_right_bracket_inside() {
        let input = r#"<![CDATA[ Content a="]>"#;
        verify_tokenize(input, 0, &[], 0);

        let input = r#"<![CDATA[ Content a="]>other ]]"]] test ]]>Content"#;
        verify_tokenize_all(
            input,
            &[
                Ty::Cdata(Cdata::from_str(
                    r#"<![CDATA[ Content a="]>other ]]"]] test ]]>"#,
                )),
                Ty::Characters(Characters::from_str("Content")),
            ],
        );
    }

    #[test]
    fn cdata_with_single_closing_bracket() {
        let input = r#"<![CDATA[ Content a="]>]>" test ]>Content"#;
        verify_tokenize(input, 0, &[], 0);

        let input = r#"<![CDATA[ Content a="]>]>" test ]>ContentMore ]]>Real Content"#;
        verify_tokenize_all(
            input,
            &[
                Ty::Cdata(Cdata::from_str(
                    r#"<![CDATA[ Content a="]>]>" test ]>ContentMore ]]>"#,
                )),
                Ty::Characters(Characters::from_str("Real Content")),
            ],
        );
    }
}
