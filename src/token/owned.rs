// Copyright 2022 Bryant Luk
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Owned tokens have a copy of the scanned value.
//!
//! Methods that return an owned token are similar to a
//! standard `Iterator` API. While there is a performance
//! cost for each owned token having its own buffer for
//! each value, it is generally easier to write programs which
//! transforms the original document values.
//!
//! Furthermore, if the calling code needs to retain or work
//! on more than one token at a time, then it will require
//! owned tokens.

use core::str;

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::string::String;
#[cfg(feature = "std")]
use std::string::String;

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::vec::{self, Vec};
#[cfg(feature = "std")]
use std::vec::{self, Vec};

use core::fmt;

use super::prop::{Attributes, Content, Instructions, TagName, Target};

macro_rules! converters {
    ($name:ident) => {
        impl $name {
            /// All of the bytes representing the token.
            #[must_use]
            pub fn as_bytes(&self) -> &[u8] {
                &self.bytes
            }

            /// The token represented as a str.
            pub fn to_str(&self) -> Result<&str, str::Utf8Error> {
                str::from_utf8(&self.bytes)
            }
        }

        impl AsRef<[u8]> for $name {
            fn as_ref(&self) -> &[u8] {
                &self.bytes
            }
        }

        impl AsRef<Vec<u8>> for $name {
            fn as_ref(&self) -> &Vec<u8> {
                &self.bytes
            }
        }

        impl core::borrow::Borrow<[u8]> for $name {
            fn borrow(&self) -> &[u8] {
                &self.bytes
            }
        }

        impl Clone for $name {
            fn clone(&self) -> $name {
                $name {
                    bytes: self.bytes.clone(),
                }
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_struct(stringify!($name))
                    .field("bytes", &self.bytes)
                    .finish()
            }
        }

        impl core::ops::Deref for $name {
            type Target = [u8];

            fn deref(&self) -> &[u8] {
                self.bytes.deref()
            }
        }

        impl Eq for $name {}

        impl From<&[u8]> for $name {
            fn from(bytes: &[u8]) -> Self {
                Self {
                    bytes: bytes.to_vec(),
                }
            }
        }

        impl From<&mut [u8]> for $name {
            fn from(bytes: &mut [u8]) -> Self {
                Self {
                    bytes: bytes.to_vec(),
                }
            }
        }

        impl From<&str> for $name {
            fn from(str: &str) -> Self {
                Self {
                    bytes: str.as_bytes().to_vec(),
                }
            }
        }

        impl From<&Vec<u8>> for $name {
            fn from(bytes: &Vec<u8>) -> Self {
                Self {
                    bytes: bytes.clone(),
                }
            }
        }

        impl From<String> for $name {
            fn from(str: String) -> Self {
                Self {
                    bytes: Vec::from(str),
                }
            }
        }

        impl From<Vec<u8>> for $name {
            fn from(bytes: Vec<u8>) -> Self {
                Self { bytes }
            }
        }

        impl core::iter::FromIterator<u8> for $name {
            fn from_iter<I>(iter: I) -> Self
            where
                I: IntoIterator<Item = u8>,
            {
                Self {
                    bytes: Vec::from_iter(iter),
                }
            }
        }

        impl core::hash::Hash for $name {
            fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
                self.bytes.hash(state);
            }
        }

        impl<I> core::ops::Index<I> for $name
        where
            I: core::slice::SliceIndex<[u8]>,
        {
            type Output = <I as core::slice::SliceIndex<[u8]>>::Output;

            fn index(&self, index: I) -> &<$name as core::ops::Index<I>>::Output {
                self.bytes.index(index)
            }
        }

        impl IntoIterator for $name {
            type Item = u8;

            type IntoIter = vec::IntoIter<u8>;

            fn into_iter(self) -> vec::IntoIter<u8> {
                self.bytes.into_iter()
            }
        }

        impl Ord for $name {
            fn cmp(&self, other: &$name) -> core::cmp::Ordering {
                self.bytes.cmp(&other.bytes)
            }
        }

        impl PartialEq<&[u8]> for $name {
            fn eq(&self, other: &&[u8]) -> bool {
                self.bytes[..] == other[..]
            }
        }

        impl PartialEq<&mut [u8]> for $name {
            fn eq(&self, other: &&mut [u8]) -> bool {
                self.bytes[..] == other[..]
            }
        }

        impl PartialEq<Vec<u8>> for $name {
            fn eq(&self, other: &Vec<u8>) -> bool {
                self.bytes.eq(other)
            }
        }

        impl PartialEq<$name> for $name {
            fn eq(&self, other: &$name) -> bool {
                self.bytes.eq(&other.bytes)
            }
        }

        impl PartialOrd<$name> for $name {
            fn partial_cmp(&self, other: &$name) -> Option<core::cmp::Ordering> {
                self.bytes.partial_cmp(&other.bytes)
            }
        }

        impl From<$name> for Vec<u8> {
            fn from(other: $name) -> Vec<u8> {
                other.bytes
            }
        }
    };
}

/// Scanned byte values associated with a type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    /// A start tag like `<hello>`.
    StartTag(StartTag),
    /// A standalone empty element tag like `<br/>`.
    EmptyElementTag(EmptyElementTag),
    /// An end element tag like `</goodbye>`.
    EndTag(EndTag),
    /// The character content between markup.
    Characters(Characters),
    /// A processing instruction like `<?xml-stylesheet ?>`.
    ProcessingInstruction(ProcessingInstruction),
    /// A declaration like `<!DOCTYPE >`.
    Declaration(Declaration),
    /// A comment like `<!-- Example -->`.
    Comment(Comment),
    /// Character data like `<![CDATA[ Example ]]>`.
    Cdata(Cdata),
    /// The end of the file marker has been reached with all bytes successfully evaluated.
    Eof,
    /// The end of the file marker has been reached but there are leftover bytes
    /// which have not been evaluated.
    ///
    /// Usually, leftover bytes are due to reading an opening markup delimiter
    /// (e.g. `<`) but never reading a closing delimiter (e.g. `>`) before the
    /// end of the file marker is read.
    EofWithBytesNotEvaluated(BytesNotEvaluated),
}

/// A start tag for an element.
pub struct StartTag {
    bytes: Vec<u8>,
}

impl StartTag {
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
pub struct EmptyElementTag {
    bytes: Vec<u8>,
}

impl EmptyElementTag {
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
pub struct EndTag {
    bytes: Vec<u8>,
}

impl EndTag {
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
pub struct Characters {
    bytes: Vec<u8>,
}

impl Characters {
    /// The text content of the characters.
    #[must_use]
    pub fn content(&self) -> Content<'_> {
        Content::from(&self.bytes)
    }
}

converters!(Characters);

/// A document processing instruction.
pub struct ProcessingInstruction {
    bytes: Vec<u8>,
}

converters!(ProcessingInstruction);

impl ProcessingInstruction {
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
pub struct BytesNotEvaluated {
    bytes: Vec<u8>,
}

converters!(BytesNotEvaluated);

/// A declaration like `<!DOCTYPE >`.
pub struct Declaration {
    bytes: Vec<u8>,
}

converters!(Declaration);

/// A comment like `<!-- Example -->`.
pub struct Comment {
    bytes: Vec<u8>,
}

converters!(Comment);

/// Character data like `<![CDATA[ Example ]]>`.
pub struct Cdata {
    bytes: Vec<u8>,
}

impl Cdata {
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
        assert_eq!(AsRef::<[u8]>::as_ref(&start_tag), "<abc>".as_bytes());
        assert_eq!(start_tag.as_bytes(), "<abc>".as_bytes());
    }

    #[test]
    fn start_tag_from() -> Result<()> {
        use core::convert::From;

        let expected = b"<abc>";
        let start_tag = StartTag::from(&expected[..]);
        assert_eq!(start_tag.to_str()?, "<abc>");

        let mut expected = Vec::new();
        expected.extend_from_slice(b"<abc>");
        let start_tag = StartTag::from(&expected);
        assert_eq!(start_tag.to_str()?, "<abc>");

        let expected = "<abc>";
        let start_tag = StartTag::from(expected);
        assert_eq!(start_tag.to_str()?, "<abc>");

        let expected = String::from("<abc>");
        let start_tag = StartTag::from(expected);
        assert_eq!(start_tag.to_str()?, "<abc>");
        Ok(())
    }

    #[test]
    fn start_tag_partial_eq() -> Result<()> {
        let start_tag = StartTag::from(b"<abc>".as_ref());
        assert_eq!(start_tag.to_str()?, "<abc>");
        assert_eq!(start_tag, &b"<abc>"[..]);
        assert_eq!(start_tag.to_str()?, String::from("<abc>"));
        assert_eq!(start_tag, "<abc>".as_bytes());
        assert_eq!(start_tag, "<abc>".as_bytes().to_vec());

        assert!(start_tag == "<abc>".as_bytes());
        assert!(start_tag == "<abc>".as_bytes().to_vec());

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
