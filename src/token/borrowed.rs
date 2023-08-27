// Copyright 2022 Bryant Luk
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Borrowed tokens re-use an internal buffer to represent a scanned value.
//!
//! Methods that return borrowed tokens act like a cursor API which is
//! generally more efficient to read the XML. However, in order to
//! store the value in a separate variable/field, then the bytes will
//! need to be copied.

use core::str;

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::string::String;
#[cfg(feature = "std")]
use std::string::String;

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use std::vec::Vec;

use core::fmt;

use super::prop::{Attributes, Content, Instructions, TagName, Target};

macro_rules! converters {
    ($name:ident) => {
        impl<'a> $name<'a> {
            /// All of the bytes representing the token.
            #[must_use]
            pub fn as_bytes(&self) -> &[u8] {
                self.bytes
            }

            /// The token represented as a str.
            ///
            /// # Errors
            ///
            /// If the bytes are not a UTF-8 string.
            pub fn to_str(&self) -> Result<&str, str::Utf8Error> {
                str::from_utf8(&self.bytes)
            }

            /// The token as a new `Vec`.
            #[cfg(any(feature = "alloc", feature = "std"))]
            #[must_use]
            pub fn to_vec(&self) -> Vec<u8> {
                self.bytes.to_vec()
            }
        }

        impl<'a> AsRef<[u8]> for $name<'a> {
            fn as_ref(&self) -> &[u8] {
                self.bytes
            }
        }

        impl<'a> fmt::Debug for $name<'a> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_struct(stringify!($name))
                    .field("bytes", &self.bytes)
                    .finish()
            }
        }

        impl<'a> Eq for $name<'a> {}

        impl<'a> From<&'a [u8]> for $name<'a> {
            fn from(bytes: &'a [u8]) -> Self {
                Self { bytes }
            }
        }

        impl<'a> From<&'a mut [u8]> for $name<'a> {
            fn from(bytes: &'a mut [u8]) -> Self {
                Self { bytes }
            }
        }

        impl<'a> From<&'a str> for $name<'a> {
            fn from(str: &'a str) -> Self {
                Self {
                    bytes: str.as_bytes(),
                }
            }
        }

        #[cfg(any(feature = "alloc", feature = "std"))]
        impl<'a> From<&'a Vec<u8>> for $name<'a> {
            fn from(bytes: &'a Vec<u8>) -> Self {
                Self { bytes }
            }
        }

        #[cfg(any(feature = "alloc", feature = "std"))]
        impl<'a> From<&'a String> for $name<'a> {
            fn from(str: &'a String) -> Self {
                Self {
                    bytes: str.as_bytes(),
                }
            }
        }

        impl<'a> core::hash::Hash for $name<'a> {
            fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
                self.bytes.hash(state);
            }
        }

        impl<'a, I> core::ops::Index<I> for $name<'a>
        where
            I: core::slice::SliceIndex<[u8]>,
        {
            type Output = <I as core::slice::SliceIndex<[u8]>>::Output;

            fn index(&self, index: I) -> &<$name as core::ops::Index<I>>::Output {
                self.bytes.index(index)
            }
        }

        impl<'a> IntoIterator for $name<'a> {
            type Item = &'a u8;

            type IntoIter = core::slice::Iter<'a, u8>;

            fn into_iter(self) -> core::slice::Iter<'a, u8> {
                self.bytes.into_iter()
            }
        }

        impl<'a> Ord for $name<'a> {
            fn cmp(&self, other: &$name) -> core::cmp::Ordering {
                self.bytes.cmp(&other.bytes)
            }
        }

        impl<'a> PartialEq<[u8]> for $name<'a> {
            fn eq(&self, other: &[u8]) -> bool {
                self.bytes.eq(other)
            }
        }

        impl<'a> PartialEq<$name<'a>> for $name<'a> {
            fn eq(&self, other: &$name) -> bool {
                self.bytes.eq(other.bytes)
            }
        }

        #[cfg(any(feature = "alloc", feature = "std"))]
        impl<'a> PartialEq<Vec<u8>> for $name<'a> {
            fn eq(&self, other: &Vec<u8>) -> bool {
                self.bytes.eq(other.as_slice())
            }
        }

        impl<'a> PartialOrd<$name<'a>> for $name<'a> {
            fn partial_cmp(&self, other: &$name) -> Option<core::cmp::Ordering> {
                Some(self.bytes.cmp(other.bytes))
            }
        }
    };
}

/// Scanned byte values associated with a type.
#[derive(Debug, PartialEq, Eq)]
pub enum Token<'a> {
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
    /// The end of the file marker has been reached with all bytes successfully evaluated.
    Eof,
    /// The end of the file marker has been reached but there are leftover bytes
    /// which have not been evaluated.
    ///
    /// Usually, leftover bytes are due to reading an opening markup delimiter
    /// (e.g. `<`) but never reading a closing delimiter (e.g. `>`) before the
    /// end of the file marker is read.
    EofWithBytesNotEvaluated(BytesNotEvaluated<'a>),
}

/// A start tag for an element.
pub struct StartTag<'a> {
    bytes: &'a [u8],
}

impl<'a> StartTag<'a> {
    /// The name of the tag.
    #[must_use]
    pub fn name(&self) -> TagName<'_> {
        let index = self
            .bytes
            .iter()
            .position(|b| super::is_space(*b))
            .unwrap_or(self.bytes.len() - 1);
        TagName::from(&self.bytes[1..index])
    }

    /// The attributes of the tag.
    #[must_use]
    pub fn attributes(&self) -> Option<Attributes<'_>> {
        self.bytes
            .iter()
            .position(|b| super::is_space(*b))
            .map(|index| Attributes::from(&self.bytes[index + 1..self.bytes.len() - 1]))
    }
}

converters!(StartTag);

/// An element which has no content.
///
/// A tag like `<br/>` would be an empty element tag.
pub struct EmptyElementTag<'a> {
    bytes: &'a [u8],
}

impl<'a> EmptyElementTag<'a> {
    /// The name of the tag.
    #[must_use]
    pub fn name(&self) -> TagName<'_> {
        let index = self
            .bytes
            .iter()
            .position(|b| super::is_space(*b))
            .unwrap_or(self.bytes.len() - 2);
        TagName::from(&self.bytes[1..index])
    }

    /// The attributes of the tag.
    #[must_use]
    pub fn attributes(&self) -> Option<Attributes<'_>> {
        self.bytes
            .iter()
            .position(|b| super::is_space(*b))
            .map(|index| Attributes::from(&self.bytes[index + 1..self.bytes.len() - 2]))
    }
}

converters!(EmptyElementTag);

/// An end tag for an element.
pub struct EndTag<'a> {
    bytes: &'a [u8],
}

impl<'a> EndTag<'a> {
    /// The name of the tag.
    #[must_use]
    pub fn name(&self) -> TagName<'_> {
        let index = self
            .bytes
            .iter()
            .position(|b| super::is_space(*b))
            .unwrap_or(self.bytes.len() - 1);
        TagName::from(&self.bytes[2..index])
    }
}

converters!(EndTag);

/// Content between markup.
pub struct Characters<'a> {
    bytes: &'a [u8],
}

impl<'a> Characters<'a> {
    /// The text content of the characters.
    #[must_use]
    pub fn content(&self) -> Content<'_> {
        Content::from(self.bytes)
    }
}

converters!(Characters);

/// A document processing instruction.
pub struct ProcessingInstruction<'a> {
    bytes: &'a [u8],
}

converters!(ProcessingInstruction);

impl<'a> ProcessingInstruction<'a> {
    /// The target of the tag.
    #[must_use]
    pub fn target(&self) -> Target<'_> {
        let index = self
            .bytes
            .iter()
            .position(|b| super::is_space(*b))
            .unwrap_or(self.bytes.len() - 2);
        Target::from(&self.bytes[2..index])
    }

    /// The instructions of the tag.
    #[must_use]
    pub fn instructions(&self) -> Option<Instructions<'_>> {
        self.bytes
            .iter()
            .position(|b| super::is_space(*b))
            .map(|index| Instructions::from(&self.bytes[index + 1..self.bytes.len() - 2]))
    }
}

/// Bytes not evaluated when an end of file is reached.
///
/// If a markup starting delimiter (e.g. a `<`) was read but there was no closing
/// delimiter (e.g. `>`) read and the end of file marker is reached, then the
/// leftover bytes are enclosed.
pub struct BytesNotEvaluated<'a> {
    bytes: &'a [u8],
}

converters!(BytesNotEvaluated);

/// A declaration like `<!DOCTYPE >`.
pub struct Declaration<'a> {
    bytes: &'a [u8],
}

converters!(Declaration);

/// A comment like `<!-- Example -->`.
pub struct Comment<'a> {
    bytes: &'a [u8],
}

converters!(Comment);

/// Character data like `<![CDATA[ Example ]]>`.
pub struct Cdata<'a> {
    bytes: &'a [u8],
}

impl<'a> Cdata<'a> {
    /// The text content of the characters.
    #[must_use]
    pub fn content(&self) -> Content<'_> {
        Content::from(&self.bytes[9..self.bytes.len() - 3])
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
        let start_tag = StartTag::from(expected);
        assert_eq!(start_tag.to_str()?, "<abc>");

        Ok(())
    }

    #[test]
    #[cfg(any(feature = "alloc", feature = "std"))]
    fn start_tag_from_alloc() -> Result<()> {
        let expected = String::from("<abc>");
        let start_tag = StartTag::from(&expected);
        assert_eq!(start_tag.to_str()?, "<abc>");
        Ok(())
    }

    #[test]
    fn start_tag_partial_eq() -> Result<()> {
        let start_tag = StartTag::from(b"<abc>".as_ref());
        assert_eq!(start_tag.to_str()?, "<abc>");
        assert_eq!(start_tag, b"<abc>"[..]);
        assert_eq!(&start_tag, "<abc>".as_bytes());

        assert!(&start_tag == "<abc>".as_bytes());
        Ok(())
    }

    #[test]
    #[cfg(any(feature = "alloc", feature = "std"))]
    fn start_tag_partial_eq_alloc() -> Result<()> {
        let start_tag = StartTag::from(b"<abc>".as_ref());
        assert_eq!(start_tag.to_str()?, "<abc>");
        assert_eq!(start_tag.to_str()?, String::from("<abc>"));
        assert_eq!(start_tag, "<abc>".as_bytes().to_vec());

        assert_eq!(&start_tag, &"<abc>".as_bytes().to_vec());
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
