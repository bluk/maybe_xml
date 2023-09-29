// Copyright 2022 Bryant Luk
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Tokens are views of byte sub-slices from an input buffer.
//!
//! [`Token`] is the main type. It identifies a sub-slice of the input buffer
//! with a type of token using [`Ty`]. There are specific token types like
//! [`StartTag`] which provide a view of the bytes and provide helper methods.

#[deprecated(
    since = "0.5.0",
    note = "Use the crate's Token type to base which borrowed view type to use."
)]
pub mod borrowed;
#[cfg(any(feature = "alloc", feature = "std"))]
#[deprecated(since = "0.5.0", note = "Use Lexer with borrowed type views.")]
pub mod owned;
pub mod prop;

#[inline]
#[must_use]
const fn is_space(byte: u8) -> bool {
    matches!(byte, 32 | 9 | 13 | 10)
}

use core::str;

use prop::{Attributes, Content, Instructions, TagName, Target};

/// A slice of bytes which is identified as a specific token type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token<'a> {
    ty: Ty<'a>,
    offset: usize,
}

impl<'a> Token<'a> {
    /// Instantiates a new instance with the token type and the position where
    /// the token starts in the original input.
    #[inline]
    #[must_use]
    pub const fn new(ty: Ty<'a>, offset: usize) -> Self {
        Self { ty, offset }
    }

    /// Returns the token type.
    #[inline]
    #[must_use]
    pub const fn ty(&self) -> Ty<'a> {
        self.ty
    }

    /// Returns the byte offset in the original input where the token starts.
    #[inline]
    #[must_use]
    pub const fn offset(&self) -> usize {
        self.offset
    }

    /// Returns the length of the token in bytes.
    #[allow(clippy::len_without_is_empty)]
    #[inline]
    #[must_use]
    pub const fn len(&self) -> usize {
        self.ty.len()
    }
}

macro_rules! converters {
    ($name:ident) => {
        impl<'a> $name<'a> {
            /// Instantiates a new view with the given bytes.
            #[inline]
            #[must_use]
            pub const fn new(bytes: &'a [u8]) -> Self {
                Self(bytes)
            }

            /// All of the bytes representing the token.
            #[inline]
            #[must_use]
            pub const fn as_bytes(&self) -> &'a [u8] {
                self.0
            }

            /// The token represented as a str.
            ///
            /// # Errors
            ///
            /// If the bytes are not a UTF-8 string.
            #[inline]
            pub fn to_str(&self) -> Result<&'a str, core::str::Utf8Error> {
                core::str::from_utf8(&self.0)
            }

            /// Returns the underlying slice.
            #[inline]
            #[must_use]
            pub const fn into_inner(self) -> &'a [u8] {
                self.0
            }
        }

        impl<'a> AsRef<[u8]> for $name<'a> {
            #[inline]
            fn as_ref(&self) -> &'a [u8] {
                self.0
            }
        }

        impl<'a> From<&'a [u8]> for $name<'a> {
            #[inline]
            fn from(value: &'a [u8]) -> Self {
                Self(value)
            }
        }

        impl<'a> From<&'a str> for $name<'a> {
            #[inline]
            fn from(value: &'a str) -> Self {
                Self(value.as_bytes())
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

impl<'a> Ty<'a> {
    /// Returns the slice of bytes identified as part of the token.
    #[must_use]
    pub const fn as_bytes(&self) -> &'a [u8] {
        match self {
            Ty::StartTag(v) => v.as_bytes(),
            Ty::EmptyElementTag(v) => v.as_bytes(),
            Ty::EndTag(v) => v.as_bytes(),
            Ty::Characters(v) => v.as_bytes(),
            Ty::ProcessingInstruction(v) => v.as_bytes(),
            Ty::Declaration(v) => v.as_bytes(),
            Ty::Comment(v) => v.as_bytes(),
            Ty::Cdata(v) => v.as_bytes(),
        }
    }

    /// Returns the length of the token in bytes.
    #[allow(clippy::len_without_is_empty)]
    #[inline]
    #[must_use]
    pub const fn len(&self) -> usize {
        self.as_bytes().len()
    }
}

/// A start tag for an element.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StartTag<'a>(&'a [u8]);

impl<'a> StartTag<'a> {
    /// The name of the tag.
    #[must_use]
    pub fn name(&self) -> TagName<'a> {
        let index = self
            .0
            .iter()
            .position(|b| is_space(*b))
            .unwrap_or(self.0.len() - 1);
        TagName::from(&self.0[1..index])
    }

    /// The attributes of the tag.
    #[must_use]
    pub fn attributes(&self) -> Option<Attributes<'a>> {
        self.0
            .iter()
            .position(|b| is_space(*b))
            .map(|index| Attributes::from(&self.0[index + 1..self.0.len() - 1]))
    }
}

converters!(StartTag);

/// An element which has no content.
///
/// A tag like `<br/>` would be an empty element tag.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EmptyElementTag<'a>(&'a [u8]);

impl<'a> EmptyElementTag<'a> {
    /// The name of the tag.
    #[must_use]
    pub fn name(&self) -> TagName<'a> {
        let index = self
            .0
            .iter()
            .position(|b| is_space(*b))
            .unwrap_or(self.0.len() - 2);
        TagName::from(&self.0[1..index])
    }

    /// The attributes of the tag.
    #[must_use]
    pub fn attributes(&self) -> Option<Attributes<'a>> {
        self.0
            .iter()
            .position(|b| is_space(*b))
            .map(|index| Attributes::from(&self.0[index + 1..self.0.len() - 2]))
    }
}

converters!(EmptyElementTag);

/// An end tag for an element.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EndTag<'a>(&'a [u8]);

impl<'a> EndTag<'a> {
    /// The name of the tag.
    #[must_use]
    pub fn name(&self) -> TagName<'a> {
        let index = self
            .0
            .iter()
            .position(|b| is_space(*b))
            .unwrap_or(self.0.len() - 1);
        TagName::from(&self.0[2..index])
    }
}

converters!(EndTag);

/// Content between markup.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Characters<'a>(&'a [u8]);

impl<'a> Characters<'a> {
    /// The text content of the characters.
    #[inline]
    #[must_use]
    pub const fn content(&self) -> Content<'a> {
        Content::new(self.0)
    }
}

converters!(Characters);

/// A document processing instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcessingInstruction<'a>(&'a [u8]);

converters!(ProcessingInstruction);

impl<'a> ProcessingInstruction<'a> {
    /// The target of the tag.
    #[must_use]
    pub fn target(&self) -> Target<'a> {
        let index = self
            .0
            .iter()
            .position(|b| is_space(*b))
            .unwrap_or(self.0.len() - 2);
        Target::from(&self.0[2..index])
    }

    /// The instructions of the tag.
    #[must_use]
    pub fn instructions(&self) -> Option<Instructions<'a>> {
        self.0
            .iter()
            .position(|b| is_space(*b))
            .map(|index| Instructions::from(&self.0[index + 1..self.0.len() - 2]))
    }
}

/// A declaration like `<!DOCTYPE >`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Declaration<'a>(&'a [u8]);

converters!(Declaration);

/// A comment like `<!-- Example -->`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Comment<'a>(&'a [u8]);

converters!(Comment);

/// Character data like `<![CDATA[ Example ]]>`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Cdata<'a>(&'a [u8]);

impl<'a> Cdata<'a> {
    /// The text content of the characters.
    #[inline]
    #[must_use]
    pub fn content(&self) -> Content<'a> {
        Content::new(&self.0[9..self.0.len() - 3])
    }
}

converters!(Cdata);

#[cfg(test)]
mod tests {
    use super::*;

    type Result<T> = core::result::Result<T, str::Utf8Error>;

    #[test]
    fn start_tag_as_ref() {
        let start_tag = StartTag::from(b"<abc>".as_ref());
        assert_eq!(start_tag.as_ref(), "<abc>".as_bytes());
        assert_eq!(start_tag.as_bytes(), "<abc>".as_bytes());
    }

    #[test]
    fn start_tag_from() -> Result<()> {
        use core::convert::From;

        let start_tag = StartTag::from(b"<abc>".as_ref());
        assert_eq!(start_tag.to_str()?, "<abc>");

        let start_tag = StartTag::from(b"<abc>".as_ref());
        assert_eq!(start_tag.to_str()?, "<abc>");

        let expected = "<abc>";
        let start_tag = StartTag::from(expected.as_bytes());
        assert_eq!(start_tag.to_str()?, "<abc>");

        Ok(())
    }

    #[test]
    fn start_tag_partial_eq() -> Result<()> {
        let start_tag = StartTag::from(b"<abc>".as_ref());
        assert_eq!(start_tag.to_str()?, "<abc>");
        assert_eq!(start_tag.as_ref(), &b"<abc>"[..]);
        assert_eq!(start_tag.as_bytes(), "<abc>".as_bytes());
        Ok(())
    }

    #[test]
    fn empty_start_tag_name() -> Result<()> {
        let start_tag = StartTag::from(b"<>".as_ref());
        assert_eq!(start_tag.name().as_bytes(), b"");
        assert_eq!(start_tag.name().to_str()?, "");
        Ok(())
    }

    #[test]
    fn start_tag_attributes() {
        let start_tag = StartTag::from(b"<abc attr=\"1\">".as_ref());
        assert_eq!(start_tag.attributes(), Some(Attributes::from("attr=\"1\"")));

        let start_tag = StartTag::from(b"<abc attr=\"1\" id=\"#example\">".as_ref());
        assert_eq!(
            start_tag.attributes(),
            Some(Attributes::from("attr=\"1\" id=\"#example\""))
        );
    }

    #[test]
    fn empty_empty_element_tag_name() -> Result<()> {
        let empty_element_tag = EmptyElementTag::from(b"</>".as_ref());
        assert_eq!(empty_element_tag.name().as_bytes(), b"");
        assert_eq!(empty_element_tag.name().to_str()?, "");
        Ok(())
    }

    #[test]
    fn empty_element_tag_attributes() {
        let empty_element_tag = EmptyElementTag::from(b"<abc attr=\"1\"/>".as_ref());
        assert_eq!(
            empty_element_tag.attributes(),
            Some(Attributes::from("attr=\"1\""))
        );

        let empty_element_tag =
            EmptyElementTag::from(b"<abc attr=\"1\" id=\"#example\"/>".as_ref());
        assert_eq!(
            empty_element_tag.attributes(),
            Some(Attributes::from("attr=\"1\" id=\"#example\""))
        );
    }

    #[test]
    fn empty_end_tag_name() -> Result<()> {
        let end_tag = EndTag::from(b"</>".as_ref());
        assert_eq!(end_tag.name().as_bytes(), b"");
        assert_eq!(end_tag.name().to_str()?, "");
        Ok(())
    }
}
