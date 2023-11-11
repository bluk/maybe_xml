//! Lexer for byte slice.

use crate::token::Token;

mod scanner;

use scanner::scan;

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
/// let ty = lexer.tokenize(&mut pos).map(|token| token.ty());
/// assert_eq!(Some(Ty::StartTag(StartTag::from_str("<id>"))), ty);
///
/// // Position was assigned to the index after the end of the token
/// assert_eq!(4, pos);
///
/// let ty = lexer.tokenize(&mut pos).map(|token| token.ty());
/// assert_eq!(Some(Ty::Characters(Characters::from_str("123"))), ty);
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
/// let ty = lexer.tokenize(&mut pos).map(|token| token.ty());
/// assert_eq!(Some(Ty::EndTag(EndTag::from_str("</id>"))), ty);
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    /// let ty = lexer.tokenize(&mut pos).map(|token| token.ty());
    /// assert_eq!(Some(Ty::StartTag(StartTag::from_str("<id>"))), ty);
    ///
    /// // Position was assigned to the index after the end of the token
    /// assert_eq!(4, pos);
    ///```
    #[must_use]
    pub fn tokenize(&self, pos: &mut usize) -> Option<Token<'a>> {
        // The &mut function parameter is blocking this function from being const
        // See https://github.com/rust-lang/rust/issues/57349
        // On some micro-benchmarks, there was a drop in performance when
        // removing the assignment of pos and having the caller modify the
        // position:
        //
        // ```
        // let lexer = Lexer::from_str("<hello>");
        // let mut pos = 0;
        // let token = lexer.tokenize(pos)?;
        // pos = 0 + token.len();
        // ```
        //
        // A separate function could be made avaialble which is const if someone wants it.

        if let Some(end) = scan(self.input, *pos) {
            // This is a convoluted but *const* way of getting &self.input[*pos..end]
            let (bytes, _) = self.input.split_at(end);
            let (_, bytes) = bytes.split_at(*pos);
            let token = Token::from_str(unsafe { core::str::from_utf8_unchecked(bytes) });

            *pos = end;
            Some(token)
        } else {
            None
        }
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
    /// let token = iter.next();
    /// assert_eq!(Some(Ty::StartTag(StartTag::from_str("<id>"))), token.map(|t| t.ty()));
    ///
    /// let pos = pos + token.map(|t| t.len()).unwrap_or_default();
    ///
    /// let token = iter.next();
    /// assert_eq!(Some(Ty::Characters(Characters::from_str("123"))), token.map(|t| t.ty()));
    ///
    /// let pos = pos + token.map(|t| t.len()).unwrap_or_default();
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
    /// let token = iter.next();
    /// assert_eq!(Some(Ty::EndTag(EndTag::from_str("</id>"))), token.map(|t| t.ty()));
    ///
    /// let pos = pos + token.map(|t| t.len()).unwrap_or_default();
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
    #[must_use]
    pub const fn iter(&self, pos: usize) -> Iter<'a> {
        Iter::new(*self, pos)
    }

    /// Return the underlying bytes being tokenized.
    #[inline]
    #[must_use]
    pub const fn into_inner(self) -> &'a [u8] {
        self.input
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

/// The returned iterator type for [`Lexer::iter()`].
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
/// let mut iter = lexer.iter(4)
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
/// # Ok::<(), std::io::Error>(())
/// ```
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Iter<'a> {
    inner: Lexer<'a>,
    pos: usize,
}

impl<'a> Iter<'a> {
    #[inline]
    #[must_use]
    const fn new(inner: Lexer<'a>, pos: usize) -> Self {
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
/// # Ok::<(), std::io::Error>(())
/// ```
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct IntoIter<'a> {
    inner: Lexer<'a>,
    pos: usize,
}

impl<'a> IntoIter<'a> {
    #[inline]
    #[must_use]
    const fn new(inner: Lexer<'a>, pos: usize) -> Self {
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
        let lexer = Lexer::from_str("");
        let mut pos = 0;
        assert_eq!(None, lexer.tokenize(&mut pos));
        assert_eq!(0, pos);
    }

    #[test]
    fn pos_greater_than_slice_len() {
        let lexer = Lexer::from_str("");
        let mut pos = 1;
        assert_eq!(None, lexer.tokenize(&mut pos));
        assert_eq!(pos, 1);
    }

    #[test]
    fn pos_greater_than_slice_len_2() {
        let lexer = Lexer::from_str("hello");
        let mut pos = "hello".len() + 1;
        assert_eq!(None, lexer.tokenize(&mut pos));
        assert_eq!(pos, "hello".len() + 1);
    }

    #[cfg(any(feature = "std", feature = "alloc"))]
    #[test]
    fn text_content() {
        let mut buf = Vec::new();
        let mut pos = 0;
        buf.extend("Hello".as_bytes());
        let lexer = unsafe { Lexer::from_slice_unchecked(&buf) };
        assert_eq!(Some(Token::from_str("Hello")), lexer.tokenize(&mut pos));
        assert_eq!(buf.len(), pos);

        buf.extend("wo".as_bytes());
        let lexer = unsafe { Lexer::from_slice_unchecked(&buf) };
        assert_eq!(Some(Token::from_str("wo")), lexer.tokenize(&mut pos));
        assert_eq!(buf.len(), pos);

        buf.extend("rld!<".as_bytes());
        let lexer = unsafe { Lexer::from_slice_unchecked(&buf) };
        assert_eq!(Some(Token::from_str("rld!")), lexer.tokenize(&mut pos));
        assert_eq!(buf.len() - 1, pos);
    }

    fn verify_tokenize_all(input: &str, expected: &[Ty<'_>]) {
        verify_tokenize(input, 0, expected, input.len());
    }

    fn verify_tokenize(input: &str, mut pos: usize, expected: &[Ty<'_>], end: usize) {
        let lexer = Lexer::from_str(input);

        for e in expected.iter().copied() {
            assert_eq!(Some(e), lexer.tokenize(&mut pos).map(|token| token.ty()));
        }

        assert_eq!(None, lexer.tokenize(&mut pos));
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
