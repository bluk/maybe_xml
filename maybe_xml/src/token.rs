//! Tokens are views of sub-slices from an input buffer.
//!
//! [`Token`] is the main type. It identifies a sub-slice of the input buffer
//! with a type of token using [`Ty`]. There are specific token types like
//! [`StartTag`] which provide a view of the string and provide helper methods.

pub mod prop;

#[inline]
#[must_use]
const fn is_space(byte: u8) -> bool {
    // matches!(ch, ' ' | '\t' | '\r' | '\n')
    matches!(byte, 32 | 9 | 13 | 10)
}

use core::{fmt, str};

use prop::{Attributes, Content, Instructions, TagName, Target};

/// A string which is identified as a specific token type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token<'a>(&'a str);

impl<'a> Token<'a> {
    /// Instantiates a new instance with a string.
    #[inline]
    #[must_use]
    pub(crate) const fn from_str(input: &'a str) -> Self {
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
    #[inline]
    #[must_use]
    pub const fn as_str(&self) -> &'a str {
        self.0
    }

    /// Returns the token type.
    #[inline]
    #[must_use]
    pub const fn ty(&self) -> Ty<'a> {
        let bytes = self.0.as_bytes();

        if bytes[0] != b'<' {
            return Ty::Characters(Characters(self.0));
        }

        match bytes[1] {
            b'/' => return Ty::EndTag(EndTag(self.0)),
            b'?' => return Ty::ProcessingInstruction(ProcessingInstruction(self.0)),
            b'!' => {
                match bytes[2] {
                    b'-' => {
                        if bytes[3] == b'-' {
                            return Ty::Comment(Comment(self.0));
                        }
                    }
                    b'[' => {
                        if "<![CDATA[".len() < bytes.len()
                            && bytes[3] == b'C'
                            && bytes[4] == b'D'
                            && bytes[5] == b'A'
                            && bytes[6] == b'T'
                            && bytes[7] == b'A'
                            && bytes[8] == b'['
                        {
                            return Ty::Cdata(Cdata(self.0));
                        }
                    }
                    _ => {}
                }
                return Ty::Declaration(Declaration(self.0));
            }
            _ => {
                if bytes[bytes.len() - 2] == b'/' {
                    return Ty::EmptyElementTag(EmptyElementTag(self.0));
                }
                return Ty::StartTag(StartTag(self.0));
            }
        }
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.0)
    }
}

macro_rules! converters {
    ($name:ident) => {
        impl<'a> $name<'a> {
            /// Instantiates a new view with the given string.
            #[inline]
            #[must_use]
            #[cfg(test)]
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
            #[inline]
            #[must_use]
            pub const fn as_str(&self) -> &'a str {
                self.0
            }

            /// Returns the length in bytes.
            #[allow(clippy::len_without_is_empty)]
            #[inline]
            #[must_use]
            pub const fn len(&self) -> usize {
                self.0.len()
            }
        }

        impl<'a> fmt::Display for $name<'a> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str(self.0)
            }
        }
    };
}

pub(crate) use converters;

use crate::read::parser::{self};

/// Type of token
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    pub const fn name(&self) -> TagName<'a> {
        let bytes = self.0.as_bytes();
        let Some(idx) = parser::scan_name(bytes, '<'.len_utf8()) else {
            return TagName::from_str("");
        };

        let (bytes, _) = bytes.split_at(idx);
        let (_, bytes) = bytes.split_at('<'.len_utf8());

        let value = unsafe { core::str::from_utf8_unchecked(bytes) };

        TagName::from_str(value)
    }

    /// The attributes of the tag.
    #[must_use]
    pub const fn attributes(&self) -> Option<Attributes<'a>> {
        // TODO: Should return an Attributes without Option

        let bytes = self.0.as_bytes();

        let Some(begin) = parser::scan_name(bytes, '<'.len_utf8()) else {
            return None;
        };

        let end = bytes.len() - '>'.len_utf8();

        let (bytes, _) = bytes.split_at(end);
        let (_, bytes) = bytes.split_at(begin);

        let value = unsafe { core::str::from_utf8_unchecked(bytes) };

        Some(Attributes::from_str(value))
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
    pub const fn name(&self) -> TagName<'a> {
        let bytes = self.0.as_bytes();
        let Some(idx) = parser::scan_name(bytes, '<'.len_utf8()) else {
            return TagName::from_str("");
        };

        let (bytes, _) = bytes.split_at(idx);
        let (_, bytes) = bytes.split_at('<'.len_utf8());

        let value = unsafe { core::str::from_utf8_unchecked(bytes) };

        TagName::from_str(value)
    }

    /// The attributes of the tag.
    #[must_use]
    pub const fn attributes(&self) -> Option<Attributes<'a>> {
        // TODO: Should return an Attributes without Option

        let bytes = self.0.as_bytes();

        let Some(begin) = parser::scan_name(bytes, '<'.len_utf8()) else {
            return None;
        };

        let end = bytes.len() - '/'.len_utf8() - '>'.len_utf8();

        let (bytes, _) = bytes.split_at(end);
        let (_, bytes) = bytes.split_at(begin);

        let value = unsafe { core::str::from_utf8_unchecked(bytes) };

        Some(Attributes::from_str(value))
    }
}

converters!(EmptyElementTag);

/// An end tag for an element.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EndTag<'a>(&'a str);

impl<'a> EndTag<'a> {
    /// The name of the tag.
    #[must_use]
    pub const fn name(&self) -> TagName<'a> {
        let bytes = self.0.as_bytes();

        let Some(idx) = parser::scan_name(bytes, '<'.len_utf8() + '/'.len_utf8()) else {
            return TagName::from_str("");
        };

        let (bytes, _) = bytes.split_at(idx);
        let (_, bytes) = bytes.split_at('<'.len_utf8() + '/'.len_utf8());

        let value = unsafe { core::str::from_utf8_unchecked(bytes) };

        TagName::from_str(value)
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
    pub const fn target(&self) -> Target<'a> {
        let bytes = self.0.as_bytes();

        let Some(idx) = parser::scan_name(bytes, '<'.len_utf8() + '?'.len_utf8()) else {
            return Target::from_str("");
        };

        let (bytes, _) = bytes.split_at(idx);
        let (_, bytes) = bytes.split_at('<'.len_utf8() + '?'.len_utf8());

        let value = unsafe { core::str::from_utf8_unchecked(bytes) };

        Target::from_str(value)
    }

    /// The instructions of the tag.
    #[must_use]
    pub const fn instructions(&self) -> Option<Instructions<'a>> {
        let bytes = self.0.as_bytes();

        let Some(idx) = parser::scan_name(bytes, '<'.len_utf8() + '?'.len_utf8()) else {
            return None;
        };

        let begin = parser::scan_optional_space(bytes, idx);

        let mut end = bytes.len() - '?'.len_utf8() - '>'.len_utf8() - 1;
        loop {
            if end <= begin {
                return None;
            }

            let byte = bytes[end];
            if !is_space(byte) {
                end += 1;
                break;
            }

            end -= 1;
        }

        let (bytes, _) = bytes.split_at(end);
        let (_, bytes) = bytes.split_at(begin);

        let value = unsafe { core::str::from_utf8_unchecked(bytes) };

        Some(Instructions::from_str(value))
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
    #[must_use]
    pub const fn content(&self) -> Content<'a> {
        let bytes = self.0.as_bytes();
        let (bytes, _) = bytes.split_at(self.0.len() - "]]>".len());
        let (_, bytes) = bytes.split_at("<![CDATA[".len());
        let value = unsafe { core::str::from_utf8_unchecked(bytes) };
        Content::from_str(value)
    }
}

converters!(Cdata);

#[cfg(test)]
mod tests {
    use crate::token::prop::Attribute;

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
        assert_eq!(start_tag.name().as_str(), "");
        assert_eq!(start_tag.name().as_bytes(), "".as_bytes());
        assert_eq!(start_tag.name().local().as_str(), "");
        assert_eq!(start_tag.name().namespace_prefix(), None);
        assert_eq!(start_tag.attributes(), None);
    }

    #[test]
    fn start_tag_attributes() {
        let start_tag = StartTag::from_str("<abc attr=\"1\">");
        assert_eq!(
            start_tag.attributes(),
            Some(Attributes::from_str(" attr=\"1\""))
        );

        let start_tag = StartTag::from_str("<abc attr=\"1\" id=\"#example\">");
        assert_eq!(
            start_tag.attributes(),
            Some(Attributes::from_str(" attr=\"1\" id=\"#example\""))
        );
    }

    #[test]
    fn start_tag_attributes_space() {
        let start_tag = StartTag::from_str("<abc  attr=\"1\" >");
        let attrs = start_tag.attributes().unwrap();
        assert_eq!(attrs, Attributes::from_str("  attr=\"1\" "));
        let mut attrs = attrs.into_iter();
        assert_eq!(attrs.next(), Some(Attribute::from_str("  attr=\"1\"")));
        assert_eq!(attrs.next(), None);

        let start_tag = StartTag::from_str("<abc   attr=\"1\" id=\"#example\"  >");
        let attrs = start_tag.attributes().unwrap();
        assert_eq!(
            attrs,
            Attributes::from_str("   attr=\"1\" id=\"#example\"  ")
        );
        let mut attrs = attrs.into_iter();
        assert_eq!(attrs.next(), Some(Attribute::from_str("   attr=\"1\"")));
        assert_eq!(attrs.next(), Some(Attribute::from_str(" id=\"#example\"")));
        assert_eq!(attrs.next(), None);
    }

    #[test]
    fn empty_empty_element_tag_name() {
        let empty_element_tag = EmptyElementTag::from_str("</>");
        assert_eq!(empty_element_tag.name().as_str(), "");
        assert_eq!(empty_element_tag.name().as_bytes(), "".as_bytes());
        assert_eq!(empty_element_tag.name().local().as_str(), "");
        assert_eq!(empty_element_tag.name().namespace_prefix(), None);
        assert_eq!(empty_element_tag.attributes(), None);
    }

    #[test]
    fn empty_element_tag_attributes() {
        let empty_element_tag = EmptyElementTag::from_str("<abc attr=\"1\"/>");
        assert_eq!(
            empty_element_tag.attributes(),
            Some(Attributes::from_str(" attr=\"1\""))
        );

        let empty_element_tag = EmptyElementTag::from_str("<abc attr=\"1\" id=\"#example\"/>");
        assert_eq!(
            empty_element_tag.attributes(),
            Some(Attributes::from_str(" attr=\"1\" id=\"#example\""))
        );
    }

    #[test]
    fn empty_element_tag_attributes_space() {
        let empty_element_tag = EmptyElementTag::from_str("<abc  attr=\"1\" />");
        let attrs = empty_element_tag.attributes().unwrap();
        assert_eq!(attrs, Attributes::from_str("  attr=\"1\" "));
        let mut attrs = attrs.into_iter();
        assert_eq!(attrs.next(), Some(Attribute::from_str("  attr=\"1\"")));
        assert_eq!(attrs.next(), None);

        let empty_element_tag =
            EmptyElementTag::from_str("<abc   attr=\"1\" id=\"#example\"     />");
        let attrs = empty_element_tag.attributes().unwrap();
        assert_eq!(
            attrs,
            Attributes::from_str("   attr=\"1\" id=\"#example\"     ")
        );
        let mut attrs = attrs.into_iter();
        assert_eq!(attrs.next(), Some(Attribute::from_str("   attr=\"1\"")));
        assert_eq!(attrs.next(), Some(Attribute::from_str(" id=\"#example\"")));
        assert_eq!(attrs.next(), None);
    }

    #[test]
    fn empty_end_tag_name() {
        let end_tag = EndTag::from_str("</>");
        assert_eq!(end_tag.name().as_str(), "");
        assert_eq!(end_tag.name().as_bytes(), "".as_bytes());
        assert_eq!(end_tag.name().local().as_str(), "");
        assert_eq!(end_tag.name().namespace_prefix(), None);
    }

    #[test]
    fn pi_no_content() {
        let pi = ProcessingInstruction::from_str("<??>");
        assert_eq!(Target::from_str(""), pi.target());
        assert_eq!(None, pi.instructions());
    }

    #[test]
    fn pi_no_spaces_content() {
        let pi = ProcessingInstruction::from_str("<?xml-stylesheet?>");
        assert_eq!(Target::from_str("xml-stylesheet"), pi.target());
        assert_eq!(None, pi.instructions());
    }

    #[test]
    fn pi_spaces_no_content() {
        let pi = ProcessingInstruction::from_str("<?xml-stylesheet ?>");
        assert_eq!(Target::from_str("xml-stylesheet"), pi.target());
        assert_eq!(None, pi.instructions());
    }

    #[test]
    fn pi_spaces_content_space() {
        let pi = ProcessingInstruction::from_str("<?xml-stylesheet  id=\"1\" test ?>");
        assert_eq!(Target::from_str("xml-stylesheet"), pi.target());
        assert_eq!(
            Some(Instructions::from_str("id=\"1\" test")),
            pi.instructions()
        );

        let pi = ProcessingInstruction::from_str("<?xml-stylesheet     id=\"1\" test    ?>");
        assert_eq!(Target::from_str("xml-stylesheet"), pi.target());
        assert_eq!(
            Some(Instructions::from_str("id=\"1\" test")),
            pi.instructions()
        );
    }

    #[test]
    fn pi_spaces_content_no_trailing_space() {
        let pi = ProcessingInstruction::from_str("<?xml-stylesheet id=\"1\" test?>");
        assert_eq!(Target::from_str("xml-stylesheet"), pi.target());
        assert_eq!(
            Some(Instructions::from_str("id=\"1\" test")),
            pi.instructions()
        );
    }
}
