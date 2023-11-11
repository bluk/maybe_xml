//! Tokens are views of byte sub-slices from an input buffer.
//!
//! [`Token`] is the main type. It identifies a sub-slice of the input buffer
//! with a type of token using [`Ty`]. There are specific token types like
//! [`StartTag`] which provide a view of the bytes and provide helper methods.

pub mod prop;

#[inline]
#[must_use]
const fn is_space_ch(ch: char) -> bool {
    matches!(ch, ' ' | '\t' | '\r' | '\n')
}

use core::str;

use prop::{Attributes, Content, Instructions, TagName, Target};

/// A slice of bytes which is identified as a specific token type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token<'a>(&'a str);

impl<'a> Token<'a> {
    /// Instantiates a new instance with a string.
    #[inline]
    #[must_use]
    pub const fn from_str(input: &'a str) -> Self {
        Self(input)
    }

    /// Returns the slice of bytes identified as part of the token.
    #[inline]
    #[must_use]
    pub const fn as_bytes(&self) -> &'a [u8] {
        self.0.as_bytes()
    }

    /// Returns the length of the token in bytes.
    #[allow(clippy::len_without_is_empty)]
    #[inline]
    #[must_use]
    pub const fn len(&self) -> usize {
        self.0.len()
    }

    /// The token represented as a str.
    ///
    /// # Errors
    ///
    /// If the bytes are not a UTF-8 string.
    #[inline]
    #[must_use]
    pub const fn as_str(&self) -> &'a str {
        self.0
    }

    /// Returns the token type.
    #[inline]
    #[must_use]
    pub fn ty(&self) -> Ty<'a> {
        // The method could be `const` but the implementation could also be
        // changed to use the unsafe `get_unchecked` method on the bytes (which
        // is not const).  There is a slight gain between 3 to 6% in some
        // micro-benchmark tests.

        let mut chars = self.0.chars();

        if chars.next() != Some('<') {
            return Ty::Characters(Characters(self.0));
        }

        match chars.next() {
            Some('/') => return Ty::EndTag(EndTag(self.0)),
            Some('?') => return Ty::ProcessingInstruction(ProcessingInstruction(self.0)),
            Some('!') => {
                match chars.next() {
                    Some('-') => {
                        if chars.next() == Some('-') {
                            return Ty::Comment(Comment(self.0));
                        }
                    }
                    Some('[') => {
                        if self.0.len() > "<![CDATA[".len()
                            && chars.next() == Some('C')
                            && chars.next() == Some('D')
                            && chars.next() == Some('A')
                            && chars.next() == Some('T')
                            && chars.next() == Some('A')
                            && chars.next() == Some('[')
                        {
                            return Ty::Cdata(Cdata(self.0));
                        }
                    }
                    _ => {}
                }
                return Ty::Declaration(Declaration(self.0));
            }
            _ => {
                if chars.nth_back(1) == Some('/') {
                    return Ty::EmptyElementTag(EmptyElementTag(self.0));
                }
                return Ty::StartTag(StartTag(self.0));
            }
        }
    }
}

impl<'a> From<&'a str> for Token<'a> {
    #[inline]
    fn from(value: &'a str) -> Self {
        Self(value)
    }
}

macro_rules! converters {
    ($name:ident) => {
        impl<'a> $name<'a> {
            /// Instantiates a new view with the given string.
            #[inline]
            #[must_use]
            pub const fn from_str(value: &'a str) -> Self {
                Self(value)
            }

            /// All of the bytes representing the token.
            #[inline]
            #[must_use]
            pub const fn as_bytes(&self) -> &'a [u8] {
                self.0.as_bytes()
            }

            /// The token represented as a str.
            ///
            /// # Errors
            ///
            /// If the bytes are not a UTF-8 string.
            #[inline]
            #[must_use]
            pub const fn as_str(&self) -> &'a str {
                self.0
            }
        }

        impl<'a> From<&'a str> for $name<'a> {
            #[inline]
            fn from(value: &'a str) -> Self {
                Self(value)
            }
        }
    };
}
pub(crate) use converters;

/// Type of token
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ty<'a> {
    /// A start tag like `<hello>`.
    StartTag(StartTag<'a>),
    /// A standalone empty element tag like `<br/>`.
    EmptyElementTag(EmptyElementTag<'a>),
    /// An end element tag like `</goodbye>`.
    EndTag(EndTag<'a>),
    /// The character content between markup.
    Characters(Characters<'a>),
    /// A processing instruction like `<?xml-stylesheet ?>`.
    ProcessingInstruction(ProcessingInstruction<'a>),
    /// A declaration like `<!DOCTYPE >`.
    Declaration(Declaration<'a>),
    /// A comment like `<!-- Example -->`.
    Comment(Comment<'a>),
    /// Character data like `<![CDATA[ Example ]]>`.
    Cdata(Cdata<'a>),
}

/// A start tag for an element.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StartTag<'a>(&'a str);

impl<'a> StartTag<'a> {
    /// The name of the tag.
    #[must_use]
    pub fn name(&self) -> TagName<'a> {
        let index = self
            .0
            .char_indices()
            .find_map(|(pos, ch)| is_space_ch(ch).then(|| pos))
            .unwrap_or(self.0.len() - '>'.len_utf8());
        TagName::from_str(&self.0['<'.len_utf8()..index])
    }

    /// The attributes of the tag.
    #[must_use]
    pub fn attributes(&self) -> Option<Attributes<'a>> {
        self.0
            .char_indices()
            .find(|(_, ch)| is_space_ch(*ch))
            .map(|(index, ch)| {
                Attributes::from_str(&self.0[index + ch.len_utf8()..self.0.len() - '>'.len_utf8()])
            })
    }
}

converters!(StartTag);

/// An element which has no content.
///
/// A tag like `<br/>` would be an empty element tag.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EmptyElementTag<'a>(&'a str);

impl<'a> EmptyElementTag<'a> {
    /// The name of the tag.
    #[must_use]
    pub fn name(&self) -> TagName<'a> {
        let index = self
            .0
            .char_indices()
            .find_map(|(pos, ch)| is_space_ch(ch).then(|| pos))
            .unwrap_or(self.0.len() - '/'.len_utf8() - '>'.len_utf8());
        TagName::from_str(&self.0['<'.len_utf8()..index])
    }

    /// The attributes of the tag.
    #[must_use]
    pub fn attributes(&self) -> Option<Attributes<'a>> {
        self.0
            .char_indices()
            .find(|(_, ch)| is_space_ch(*ch))
            .map(|(index, ch)| {
                Attributes::from_str(
                    &self.0[index + ch.len_utf8()..self.0.len() - '/'.len_utf8() - '>'.len_utf8()],
                )
            })
    }
}

converters!(EmptyElementTag);

/// An end tag for an element.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EndTag<'a>(&'a str);

impl<'a> EndTag<'a> {
    /// The name of the tag.
    #[must_use]
    pub fn name(&self) -> TagName<'a> {
        let index = self
            .0
            .char_indices()
            .find_map(|(pos, ch)| is_space_ch(ch).then(|| pos))
            .unwrap_or(self.0.len() - '>'.len_utf8());
        TagName::from_str(&self.0['<'.len_utf8() + '/'.len_utf8()..index])
    }
}

converters!(EndTag);

/// Content between markup.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Characters<'a>(&'a str);

impl<'a> Characters<'a> {
    /// The text content of the characters.
    #[inline]
    #[must_use]
    pub const fn content(&self) -> Content<'a> {
        Content::from_str(self.0)
    }
}

converters!(Characters);

/// A document processing instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcessingInstruction<'a>(&'a str);

converters!(ProcessingInstruction);

impl<'a> ProcessingInstruction<'a> {
    /// The target of the tag.
    #[must_use]
    pub fn target(&self) -> Target<'a> {
        let index = self
            .0
            .char_indices()
            .find_map(|(pos, ch)| is_space_ch(ch).then(|| pos))
            .unwrap_or(self.0.len() - '?'.len_utf8() - '>'.len_utf8());
        Target::from_str(&self.0['<'.len_utf8() + '?'.len_utf8()..index])
    }

    /// The instructions of the tag.
    #[must_use]
    pub fn instructions(&self) -> Option<Instructions<'a>> {
        self.0
            .char_indices()
            .find(|(_, ch)| is_space_ch(*ch))
            .map(|(index, ch)| {
                Instructions::from_str(
                    &self.0[index + ch.len_utf8()..self.0.len() - '?'.len_utf8() - '>'.len_utf8()],
                )
            })
    }
}

/// A declaration like `<!DOCTYPE >`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Declaration<'a>(&'a str);

converters!(Declaration);

/// A comment like `<!-- Example -->`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Comment<'a>(&'a str);

converters!(Comment);

/// Character data like `<![CDATA[ Example ]]>`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Cdata<'a>(&'a str);

impl<'a> Cdata<'a> {
    /// The text content of the characters.
    #[inline]
    #[must_use]
    pub fn content(&self) -> Content<'a> {
        Content::from_str(&self.0["<![CDATA[".len()..self.0.len() - "]]>".len()])
    }
}

converters!(Cdata);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn start_tag_as_ref() {
        let start_tag = StartTag::from_str("<abc>");
        assert_eq!(start_tag.as_bytes(), "<abc>".as_bytes());
    }

    #[test]
    fn start_tag_from() {
        let start_tag = StartTag::from_str("<abc>");
        assert_eq!(start_tag.as_str(), "<abc>");

        let start_tag = StartTag::from_str("<abc>");
        assert_eq!(start_tag.as_str(), "<abc>");

        let expected = "<abc>";
        let start_tag = StartTag::from_str(expected);
        assert_eq!(start_tag.as_str(), "<abc>");
    }

    #[test]
    fn start_tag_partial_eq() {
        let start_tag = StartTag::from_str("<abc>");
        assert_eq!(start_tag.as_str(), "<abc>");
        assert_eq!(start_tag.as_bytes(), "<abc>".as_bytes());
    }

    #[test]
    fn empty_start_tag_name() {
        let start_tag = StartTag::from_str("<>");
        assert_eq!(start_tag.name().as_bytes(), b"");
        assert_eq!(start_tag.name().as_str(), "");
    }

    #[test]
    fn start_tag_attributes() {
        let start_tag = StartTag::from_str("<abc attr=\"1\">");
        assert_eq!(
            start_tag.attributes(),
            Some(Attributes::from_str("attr=\"1\""))
        );

        let start_tag = StartTag::from_str("<abc attr=\"1\" id=\"#example\">");
        assert_eq!(
            start_tag.attributes(),
            Some(Attributes::from_str("attr=\"1\" id=\"#example\""))
        );
    }

    #[test]
    fn empty_empty_element_tag_name() {
        let empty_element_tag = EmptyElementTag::from_str("</>");
        assert_eq!(empty_element_tag.name().as_bytes(), b"");
        assert_eq!(empty_element_tag.name().as_str(), "");
    }

    #[test]
    fn empty_element_tag_attributes() {
        let empty_element_tag = EmptyElementTag::from_str("<abc attr=\"1\"/>");
        assert_eq!(
            empty_element_tag.attributes(),
            Some(Attributes::from_str("attr=\"1\""))
        );

        let empty_element_tag = EmptyElementTag::from_str("<abc attr=\"1\" id=\"#example\"/>");
        assert_eq!(
            empty_element_tag.attributes(),
            Some(Attributes::from("attr=\"1\" id=\"#example\""))
        );
    }

    #[test]
    fn empty_end_tag_name() {
        let end_tag = EndTag::from_str("</>");
        assert_eq!(end_tag.name().as_bytes(), b"");
        assert_eq!(end_tag.name().as_str(), "");
    }
}
