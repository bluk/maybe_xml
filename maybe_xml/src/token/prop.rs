//! Properties are borrowed byte slice views into tokens.
//!
//! While tokens associate a byte sequence with a type such as a start tag or CDATA,
//! there may be interesting "properties" within the token such as the name of the
//! tag or the contents of a Characters/CDATA token.

use crate::bytes::QuoteState;
use core::str;

macro_rules! converters {
    ($name:ident) => {
        impl<'a> $name<'a> {
            /// Instantiates a new instance from an unsafe slice of bytes.
            ///
            /// # Safety
            ///
            /// The bytes are assumed to be a valid UTF-8 string. If the bytes
            /// are not a UTF-8 string, behavior is undefined.
            #[inline]
            #[must_use]
            pub const unsafe fn from_slice(bytes: &'a [u8]) -> Self {
                Self(bytes)
            }

            /// Instantiates a new instance with a string.
            #[inline]
            #[must_use]
            pub const fn from_str(input: &'a str) -> Self {
                Self(input.as_bytes())
            }

            /// All of the bytes representing the token property.
            #[inline]
            #[must_use]
            pub const fn as_bytes(&self) -> &'a [u8] {
                self.0
            }

            /// The token property represented as a str.
            ///
            /// # Errors
            ///
            /// If the bytes are not a UTF-8 string.
            #[inline]
            pub fn to_str(&self) -> Result<&'a str, str::Utf8Error> {
                // Cannot be const until MSRV is at least 1.63.0
                str::from_utf8(&self.0)
            }

            /// The token property represented as a str.
            ///
            /// # Safety
            ///
            /// The underlying bytes are assumed to be UTF-8. If the bytes are
            /// not valid UTF-8, then the behavior is undefined.
            #[inline]
            #[must_use]
            pub const unsafe fn to_str_unchecked(&self) -> &'a str {
                core::str::from_utf8_unchecked(&self.0)
            }

            /// Returns the underlying slice.
            #[inline]
            #[must_use]
            pub const fn into_inner(self) -> &'a [u8] {
                self.0
            }
        }

        impl<'a> From<&'a str> for $name<'a> {
            fn from(value: &'a str) -> Self {
                Self(value.as_bytes())
            }
        }
    };
}

/// The name of the tag (e.g. `name` in `<name>` or `</name>`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TagName<'a>(&'a [u8]);

impl<'a> TagName<'a> {
    /// The local part of the name.
    ///
    /// For example, if `xml:example` was the tag name, then `example` would be the local part of the name.
    /// If there is no namespace prefix, the entire name is returned.
    #[must_use]
    pub fn local(&self) -> LocalName<'a> {
        if let Some(index) = self.0.iter().position(|b| *b == b':') {
            LocalName(&self.0[index + 1..])
        } else {
            LocalName(self.0)
        }
    }

    /// The namespace prefix if available.
    ///
    /// For example if `xml:example` was the tag name, then `xml` would be the namespace prefix.
    #[must_use]
    pub fn namespace_prefix(&self) -> Option<NamespacePrefix<'a>> {
        self.0
            .iter()
            .position(|b| *b == b':')
            .map(|index| NamespacePrefix(&self.0[..index]))
    }
}

converters!(TagName);

/// The local part of the name.
///
/// For example, if `xml:example` was the tag name, then `example` would be the local part of the name.
/// If there is no namespace prefix, the entire name is returned.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalName<'a>(&'a [u8]);

converters!(LocalName);

/// The namespace prefix if available.
///
/// For example if `xml:namespace` was the tag name, then `xml` would be the namespace prefix.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NamespacePrefix<'a>(&'a [u8]);

converters!(NamespacePrefix);

/// All of the attribute bytes of a tag.
///
/// For the vast majority of use cases, a library user should call `iter()` or `.into_iter()`.
///
/// The bytes may include additional spacing in the raw byte form.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Attributes<'a>(&'a [u8]);

impl<'a> IntoIterator for Attributes<'a> {
    type Item = Attribute<'a>;
    type IntoIter = AttributeIntoIter<'a>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        AttributeIntoIter {
            bytes: self.0,
            index: 0,
        }
    }
}

converters!(Attributes);

#[must_use]
fn iter_attr(mut index: usize, bytes: &[u8]) -> (usize, Option<Attribute<'_>>) {
    if index == bytes.len() {
        return (index, None);
    }

    if let Some(begin) = bytes[index..].iter().position(|b| !super::is_space(*b)) {
        let mut saw_space_before_equals = false;
        let mut last_nonspace_index_before_equals = None;
        let mut saw_equals = false;
        let mut saw_characters_after_equals = false;
        let mut quote_state = QuoteState::None;
        for (loop_index, &byte) in bytes[index + begin..].iter().enumerate() {
            match byte {
                b'"' => match quote_state {
                    QuoteState::None => quote_state = QuoteState::Double,
                    QuoteState::Single => {}
                    QuoteState::Double => {
                        if saw_equals {
                            let attr =
                                Attribute(&bytes[(index + begin)..=(index + begin + loop_index)]);
                            index += begin + loop_index + 1;
                            return (index, Some(attr));
                        }
                    }
                },
                b'\'' => match quote_state {
                    QuoteState::None => quote_state = QuoteState::Single,
                    QuoteState::Single => {
                        if saw_equals {
                            let attr =
                                Attribute(&bytes[(index + begin)..=(index + begin + loop_index)]);
                            index += begin + loop_index + 1;
                            return (index, Some(attr));
                        }
                    }
                    QuoteState::Double => {}
                },
                b'=' => saw_equals = true,
                b if super::is_space(b) => {
                    if !saw_equals {
                        saw_space_before_equals = true;
                    } else if saw_characters_after_equals {
                        match quote_state {
                            QuoteState::None => {
                                let attr =
                                    Attribute(&bytes[index + begin..index + begin + loop_index]);
                                index += begin + loop_index + 1;
                                return (index, Some(attr));
                            }
                            QuoteState::Single | QuoteState::Double => {}
                        }
                    }
                }
                _ => {
                    if saw_equals {
                        saw_characters_after_equals = true;
                    } else if saw_space_before_equals {
                        if let Some(last_seen_char) = last_nonspace_index_before_equals {
                            let attr = Attribute(
                                &bytes[(index + begin)..=(index + begin + last_seen_char)],
                            );
                            index += begin + loop_index;
                            return (index, Some(attr));
                        }

                        let attr = Attribute(&bytes[index + begin..index + begin + loop_index]);
                        index += begin + loop_index;
                        return (index, Some(attr));
                    } else {
                        last_nonspace_index_before_equals = Some(loop_index);
                    }
                }
            }
        }
        let attr = Attribute(&bytes[index + begin..]);
        index = bytes.len();
        (index, Some(attr))
    } else {
        index = bytes.len();
        (index, None)
    }
}

/// An iterator which returns an individual attribute on every `next()` call.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AttributeIntoIter<'a> {
    bytes: &'a [u8],
    index: usize,
}

impl<'a> Iterator for AttributeIntoIter<'a> {
    type Item = Attribute<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (new_index, item) = iter_attr(self.index, self.bytes);
        self.index = new_index;
        item
    }
}

/// The name and the optional associated value of an attribute.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Attribute<'a>(&'a [u8]);

impl<'a> Attribute<'a> {
    /// The attribute's name.
    #[must_use]
    pub fn name(&self) -> AttributeName<'a> {
        if let Some(index) = self.0.iter().position(|b| *b == b'=') {
            if let Some(last_nonspace) = self.0[..index].iter().rposition(|b| !super::is_space(*b))
            {
                return AttributeName(&self.0[..=last_nonspace]);
            }
        }
        AttributeName(self.0)
    }

    /// The optional attribute value with the quotes removed.
    #[must_use]
    pub fn value(&self) -> Option<AttributeValue<'a>> {
        if let Some(index) = self.0.iter().position(|b| *b == b'=') {
            let mut quote_state = QuoteState::None;
            let mut begin = index + 1;
            let mut first_nonspace = None;
            let mut last_nonspace = index + 1;
            for (loop_index, &byte) in self.0[index + 1..].iter().enumerate() {
                match byte {
                    b'"' => match quote_state {
                        QuoteState::None => {
                            begin = loop_index + 1;
                            quote_state = QuoteState::Double;
                        }
                        QuoteState::Single => {}
                        QuoteState::Double => {
                            return Some(AttributeValue(
                                &self.0[index + 1 + begin..index + 1 + loop_index],
                            ));
                        }
                    },
                    b'\'' => match quote_state {
                        QuoteState::None => {
                            begin = loop_index + 1;
                            quote_state = QuoteState::Single;
                        }
                        QuoteState::Single => {
                            return Some(AttributeValue(
                                &self.0[index + 1 + begin..index + 1 + loop_index],
                            ));
                        }
                        QuoteState::Double => {}
                    },
                    b => {
                        if !super::is_space(b) {
                            last_nonspace = loop_index;
                            match first_nonspace {
                                Some(_) => {}
                                None => {
                                    first_nonspace = Some(loop_index);
                                }
                            }
                        }
                    }
                }
            }

            first_nonspace.map(|begin| {
                AttributeValue(&self.0[(index + 1 + begin)..=(index + 1 + last_nonspace)])
            })
        } else {
            None
        }
    }
}

converters!(Attribute);

/// An attribute's name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AttributeName<'a>(&'a [u8]);

impl<'a> AttributeName<'a> {
    /// The local part of the name.
    ///
    /// For example, if `xml:example` was the attribute name, then `example` would be the local part of the name.
    /// If there is no namespace prefix, the entire name is returned.
    #[must_use]
    pub fn local(&self) -> LocalName<'a> {
        self.0.iter().position(|b| *b == b':').map_or_else(
            || LocalName(self.0),
            |index| LocalName(&self.0[index + 1..]),
        )
    }

    /// The namespace prefix if available.
    ///
    /// For example if `xml:example` was the attribute name, then `xml` would be the namespace prefix.
    #[must_use]
    pub fn namespace_prefix(&self) -> Option<NamespacePrefix<'a>> {
        self.0
            .iter()
            .position(|b| *b == b':')
            .map(|index| NamespacePrefix(&self.0[..index]))
    }
}

converters!(AttributeName);

/// An attribute's value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AttributeValue<'a>(&'a [u8]);

converters!(AttributeValue);

/// The character content.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Content<'a>(&'a [u8]);

converters!(Content);

/// The target of the processing instruction (e.g. `xml` in `<?xml ?>`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Target<'a>(&'a [u8]);

converters!(Target);

/// The content of the processing instruction (e.g. `encoding="utf8"` in `<?xml encoding="utf-8"?>`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Instructions<'a>(&'a [u8]);

converters!(Instructions);

#[cfg(test)]
mod tests {
    use super::*;

    type Result<T> = core::result::Result<T, str::Utf8Error>;

    #[test]
    fn tag_name() -> Result<()> {
        let tag_name = TagName(b"br");
        assert_eq!(tag_name.as_bytes(), b"br");
        assert_eq!(tag_name.to_str()?, "br");
        Ok(())
    }

    #[test]
    fn tag_name_with_namespace_prefix() -> Result<()> {
        let tag_name = TagName(b"customer:id");
        assert_eq!(tag_name.local(), LocalName(b"id"));
        assert_eq!(tag_name.local().to_str()?, "id");
        assert_eq!(
            tag_name.namespace_prefix(),
            Some(NamespacePrefix(b"customer"))
        );
        assert_eq!(tag_name.namespace_prefix().unwrap().to_str()?, "customer");
        Ok(())
    }

    #[test]
    fn tag_name_without_namespace_prefix() -> Result<()> {
        let tag_name = TagName(b"id");
        assert_eq!(tag_name.local(), LocalName(b"id"));
        assert_eq!(tag_name.local().to_str()?, "id");
        assert_eq!(tag_name.namespace_prefix(), None);
        Ok(())
    }

    #[test]
    fn empty_attributes() {
        let attributes = Attributes(b"");
        assert_eq!(attributes.into_iter().next(), None);
    }

    #[test]
    fn attributes_single() {
        let attributes = Attributes(b"attr=\"1\"");
        let mut attributes_iter = attributes.into_iter();
        assert_eq!(attributes_iter.next(), Some(Attribute(b"attr=\"1\"")));
        assert_eq!(attributes_iter.next(), None);

        let mut attributes_into_iter = attributes.into_iter();
        assert_eq!(attributes_into_iter.next(), Some(Attribute(b"attr=\"1\"")));
        assert_eq!(attributes_into_iter.next(), None);
    }

    #[test]
    fn attributes_single_with_spaces() {
        let attributes = Attributes(b"   attr=\"1 example\" ");
        let mut attributes_iter = attributes.into_iter();
        assert_eq!(
            attributes_iter.next(),
            Some(Attribute(b"attr=\"1 example\""))
        );
        assert_eq!(attributes_iter.next(), None);

        let mut attributes_into_iter = attributes.into_iter();
        assert_eq!(
            attributes_into_iter.next(),
            Some(Attribute(b"attr=\"1 example\""))
        );
        assert_eq!(attributes_into_iter.next(), None);
    }

    #[test]
    fn attributes_multiple() {
        let attributes = Attributes(b"attr=\"1\" id='test' name=invalid name=\"multiple example\"");
        let mut attributes_iter = attributes.into_iter();
        assert_eq!(attributes_iter.next(), Some(Attribute(b"attr=\"1\"")));
        assert_eq!(attributes_iter.next(), Some(Attribute(b"id='test'")));
        assert_eq!(attributes_iter.next(), Some(Attribute(b"name=invalid")));
        assert_eq!(
            attributes_iter.next(),
            Some(Attribute(b"name=\"multiple example\""))
        );
        assert_eq!(attributes_iter.next(), None);

        let mut attributes_into_iter = attributes.into_iter();
        assert_eq!(attributes_into_iter.next(), Some(Attribute(b"attr=\"1\"")));
        assert_eq!(attributes_into_iter.next(), Some(Attribute(b"id='test'")));
        assert_eq!(
            attributes_into_iter.next(),
            Some(Attribute(b"name=invalid"))
        );
        assert_eq!(
            attributes_into_iter.next(),
            Some(Attribute(b"name=\"multiple example\""))
        );
        assert_eq!(attributes_into_iter.next(), None);
    }

    #[test]
    fn attributes_multiple_with_spaces() {
        let attributes = Attributes(
            b"     attr=\"1\"  id='test' test = new   name= invalid standalone   name=\"example\" ",
        );
        let mut attributes_iter = attributes.into_iter();
        assert_eq!(attributes_iter.next(), Some(Attribute(b"attr=\"1\"")));
        assert_eq!(attributes_iter.next(), Some(Attribute(b"id='test'")));
        assert_eq!(attributes_iter.next(), Some(Attribute(b"test = new")));
        assert_eq!(attributes_iter.next(), Some(Attribute(b"name= invalid")));
        assert_eq!(attributes_iter.next(), Some(Attribute(b"standalone")));
        assert_eq!(attributes_iter.next(), Some(Attribute(b"name=\"example\"")));
        assert_eq!(attributes_iter.next(), None);

        let mut attributes_into_iter = attributes.into_iter();
        assert_eq!(attributes_into_iter.next(), Some(Attribute(b"attr=\"1\"")));
        assert_eq!(attributes_into_iter.next(), Some(Attribute(b"id='test'")));
        assert_eq!(attributes_into_iter.next(), Some(Attribute(b"test = new")));
        assert_eq!(
            attributes_into_iter.next(),
            Some(Attribute(b"name= invalid"))
        );
        assert_eq!(attributes_into_iter.next(), Some(Attribute(b"standalone")));
        assert_eq!(
            attributes_into_iter.next(),
            Some(Attribute(b"name=\"example\""))
        );
        assert_eq!(attributes_into_iter.next(), None);
    }

    #[test]
    fn attribute_name_and_value() {
        let attribute = Attribute(b"attr=\"1\"");
        assert_eq!(attribute.name(), AttributeName(b"attr"));
        assert_eq!(attribute.value(), Some(AttributeValue(b"1")));

        let attribute = Attribute(b"id='test'");
        assert_eq!(attribute.name(), AttributeName(b"id"));
        assert_eq!(attribute.value(), Some(AttributeValue(b"test")));

        let attribute = Attribute(b"test =new");
        assert_eq!(attribute.name(), AttributeName(b"test"));
        assert_eq!(attribute.value(), Some(AttributeValue(b"new")));

        let attribute = Attribute(b"name= invalid");
        assert_eq!(attribute.name(), AttributeName(b"name"));
        assert_eq!(attribute.value(), Some(AttributeValue(b"invalid")));

        let attribute = Attribute(b"standalone");
        assert_eq!(attribute.name(), AttributeName(b"standalone"));
        assert_eq!(attribute.value(), None);

        let attribute = Attribute(b"xml:example=\"test\"");
        assert_eq!(attribute.name(), AttributeName(b"xml:example"));
        assert_eq!(attribute.value(), Some(AttributeValue(b"test")));
    }
}
