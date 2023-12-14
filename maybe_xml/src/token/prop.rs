//! Properties are borrowed byte slice views into tokens.
//!
//! While tokens associate a byte sequence with a type such as a start tag or CDATA,
//! there may be interesting "properties" within the token such as the name of the
//! tag or the contents of a Characters/CDATA token.

use crate::read::parser::{self, ScanAttributeOpts, ScanAttributeValueOpts};

use core::str;

macro_rules! converters {
    ($name:ident) => {
        impl<'a> $name<'a> {
            /// Instantiates a new instance with a string.
            #[inline]
            #[must_use]
            pub(crate) const fn from_str(value: &'a str) -> Self {
                Self(value)
            }

            /// All of the bytes representing the token property.
            #[inline]
            #[must_use]
            pub const fn as_bytes(&self) -> &'a [u8] {
                self.0.as_bytes()
            }

            /// String representing the token property.
            #[inline]
            #[must_use]
            pub const fn as_str(&self) -> &'a str {
                self.0
            }
        }
    };
}

/// The name of the tag (e.g. `name` in `<name>` or `</name>`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TagName<'a>(&'a str);

impl<'a> TagName<'a> {
    /// The local part of the name.
    ///
    /// For example, if `xml:example` was the tag name, then `example` would be
    /// the local part of the name.  If there is no namespace prefix, the entire
    /// name is returned.
    #[must_use]
    pub const fn local(&self) -> LocalName<'a> {
        let mut index = 0;
        let bytes = self.0.as_bytes();
        loop {
            if index == bytes.len() {
                return LocalName(self.0);
            }

            let byte = bytes[index];
            if byte == b':' {
                break;
            }

            index += 1;
        }

        let (_, bytes) = bytes.split_at(index + 1);

        let value = unsafe { core::str::from_utf8_unchecked(bytes) };

        LocalName::from_str(value)
    }

    /// The namespace prefix if available.
    ///
    /// For example if `xml:example` was the tag name, then `xml` would be the
    /// namespace prefix.
    #[must_use]
    pub const fn namespace_prefix(&self) -> Option<NamespacePrefix<'a>> {
        let mut index = 0;
        let bytes = self.0.as_bytes();
        loop {
            if index == bytes.len() {
                return None;
            }

            let byte = bytes[index];
            if byte == b':' {
                break;
            }

            index += 1;
        }

        let (bytes, _) = bytes.split_at(index);

        let value = unsafe { core::str::from_utf8_unchecked(bytes) };

        Some(NamespacePrefix::from_str(value))
    }
}

converters!(TagName);

/// The local part of the name.
///
/// For example, if `xml:example` was the tag name, then `example` would be the
/// local part of the name.  If there is no namespace prefix, the entire name is
/// returned.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalName<'a>(&'a str);

converters!(LocalName);

/// The namespace prefix if available.
///
/// For example if `xml:namespace` was the tag name, then `xml` would be the
/// namespace prefix.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NamespacePrefix<'a>(&'a str);

converters!(NamespacePrefix);

/// All of the attribute bytes of a tag.
///
/// For the vast majority of use cases, a library user should call `.into_iter()`.
///
/// The bytes may include additional spacing in the string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Attributes<'a>(&'a str);

impl<'a> Attributes<'a> {
    /// Instantiates a new instance with a string.
    #[inline]
    #[must_use]
    pub(crate) const fn from_str(value: &'a str) -> Self {
        Self(value)
    }

    /// Parses the attributes value into an individual attribute.
    ///
    /// # Important
    ///
    /// The `pos` should be updated with
    /// [`Attribute::len()`][Attribute::len()]. Random `pos` values are not
    /// supported and is undefined behavior.
    ///
    /// # Panics
    ///
    /// Panics if the `pos` is greater than the attributes string length or if
    /// `pos` is not at a character boundary.
    ///
    /// # Examples
    ///
    /// ```
    /// use maybe_xml::{Reader, token::{Ty, prop::Attributes}};
    ///
    /// let xml = r#"<id attr="1" id='test'>"#;
    /// # let reader = Reader::from_str(xml);
    /// # let token = reader.parse(0);
    /// #
    /// let attributes: Attributes; // get the attributes via start_tag.attributes()
    /// #
    /// # if let Some(Ty::StartTag(tag)) = token.map(|t| t.ty()) {
    /// #     attributes = tag.attributes().unwrap();
    /// # } else {
    /// #     panic!()
    /// # }
    ///
    /// let mut pos = 0;
    ///
    /// let attr = attributes.parse(pos);
    /// assert_eq!(Some("attr"), attr.map(|a| a.name().as_str()));
    /// assert_eq!(Some("1"), attr.and_then(|a| a.value().map(|val| val.as_str())));
    /// pos += attr.map(|a| a.len()).unwrap_or_default();
    ///
    /// let attr = attributes.parse(pos);
    /// assert_eq!(Some("id"), attr.map(|a| a.name().as_str()));
    /// assert_eq!(Some("test"), attr.and_then(|a| a.value().map(|val| val.as_str())));
    /// pos += attr.map(|a| a.len()).unwrap_or_default();
    ///
    /// assert_eq!(None, attributes.parse(pos));
    /// ```
    #[inline]
    #[must_use]
    pub const fn parse(&self, pos: usize) -> Option<Attribute<'a>> {
        let input = self.0.as_bytes();

        if input.len() == pos {
            return None;
        }

        assert!(
            crate::is_utf8_boundary(input[pos]),
            "pos is not at a character boundary"
        );

        if let Some(end) = iter_attr(pos, input) {
            let (bytes, _) = input.split_at(end);
            let (_, bytes) = bytes.split_at(pos);
            let value = unsafe { core::str::from_utf8_unchecked(bytes) };
            Some(Attribute(value))
        } else {
            None
        }
    }
}

impl<'a> IntoIterator for Attributes<'a> {
    type Item = Attribute<'a>;
    type IntoIter = AttributeIntoIter<'a>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        AttributeIntoIter {
            value: self.0,
            index: 0,
        }
    }
}

#[must_use]
const fn iter_attr(index: usize, bytes: &[u8]) -> Option<usize> {
    let Some(index) = parser::scan_space(bytes, index) else {
        return None;
    };

    // TODO: Adjust options to be more strict if necessary

    parser::scan_attribute(
        bytes,
        index,
        ScanAttributeOpts {
            allow_no_value: true,
            attr_value_opts: ScanAttributeValueOpts {
                allow_less_than: true,
                allow_ampersand: true,
                allow_no_quote: true,
            },
        },
    )
}

/// An iterator which returns an individual attribute on every `next()` call.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AttributeIntoIter<'a> {
    value: &'a str,
    index: usize,
}

impl<'a> Iterator for AttributeIntoIter<'a> {
    type Item = Attribute<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let bytes = self.value.as_bytes();
        let end = iter_attr(self.index, bytes)?;

        let (bytes, _) = bytes.split_at(end);
        let (_, bytes) = bytes.split_at(self.index);
        let value = unsafe { core::str::from_utf8_unchecked(bytes) };

        self.index = end;

        Some(Attribute(value))
    }
}

/// The name and the optional associated value of an attribute.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Attribute<'a>(&'a str);

impl<'a> Attribute<'a> {
    /// Instantiates a new instance with a string.
    #[inline]
    #[must_use]
    #[cfg(test)]
    pub(crate) const fn from_str(value: &'a str) -> Self {
        Self(value)
    }

    /// Returns the length of the token in bytes.
    #[allow(clippy::len_without_is_empty)]
    #[inline]
    #[must_use]
    pub const fn len(&self) -> usize {
        self.0.len()
    }

    /// The attribute's name.
    #[must_use]
    pub const fn name(&self) -> AttributeName<'a> {
        let bytes = self.0.as_bytes();
        // XXX: The spacing is already parsed in `iter_attr`, but it must be
        // parsed again because the assumption is that Attributes.len() is the
        // total space for the attribute.
        let begin = parser::scan_optional_space(bytes, 0);
        let Some(end) = parser::scan_name(bytes, begin) else {
            return AttributeName::from_str("");
        };

        let (bytes, _) = bytes.split_at(end);
        let (_, bytes) = bytes.split_at(begin);

        let value = unsafe { core::str::from_utf8_unchecked(bytes) };

        AttributeName::from_str(value)
    }

    /// The optional attribute value with the quotes removed.
    #[must_use]
    pub const fn value(&self) -> Option<AttributeValue<'a>> {
        let bytes = self.0.as_bytes();
        // XXX: The spacing is already parsed in `iter_attr`, but it must be
        // parsed again because the assumption is that Attributes.len() is the
        // total space for the attribute.
        let begin = parser::scan_optional_space(bytes, 0);
        let Some(begin) = parser::scan_name(bytes, begin) else {
            return None;
        };
        let Some(mut begin) = parser::scan_eq(bytes, begin) else {
            return None;
        };

        if bytes.len() <= begin {
            return None;
        }
        let is_quote_ch = bytes[begin] == b'"' || bytes[begin] == b'\'';

        let mut end = bytes.len();
        if is_quote_ch {
            begin += 1;
            end -= 1;
        }

        let (bytes, _) = bytes.split_at(end);
        let (_, bytes) = bytes.split_at(begin);
        let value = unsafe { core::str::from_utf8_unchecked(bytes) };
        Some(AttributeValue::from_str(value))
    }
}

/// An attribute's name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AttributeName<'a>(&'a str);

impl<'a> AttributeName<'a> {
    /// The local part of the name.
    ///
    /// For example, if `xml:example` was the attribute name, then `example`
    /// would be the local part of the name.  If there is no namespace prefix,
    /// the entire name is returned.
    #[must_use]
    pub const fn local(&self) -> LocalName<'a> {
        let mut index = 0;
        let bytes = self.0.as_bytes();
        loop {
            if index == bytes.len() {
                return LocalName(self.0);
            }

            let byte = bytes[index];
            if byte == b':' {
                break;
            }

            index += 1;
        }

        let (_, bytes) = bytes.split_at(index + 1);

        let value = unsafe { core::str::from_utf8_unchecked(bytes) };

        LocalName::from_str(value)
    }

    /// The namespace prefix if available.
    ///
    /// For example if `xml:example` was the attribute name, then `xml` would be
    /// the namespace prefix.
    #[must_use]
    pub const fn namespace_prefix(&self) -> Option<NamespacePrefix<'a>> {
        let mut index = 0;
        let bytes = self.0.as_bytes();
        loop {
            if index == bytes.len() {
                return None;
            }

            let byte = bytes[index];
            if byte == b':' {
                break;
            }

            index += 1;
        }

        let (bytes, _) = bytes.split_at(index);

        let value = unsafe { core::str::from_utf8_unchecked(bytes) };

        Some(NamespacePrefix::from_str(value))
    }
}

converters!(AttributeName);

/// An attribute's value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AttributeValue<'a>(&'a str);

converters!(AttributeValue);

/// The character content.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Content<'a>(&'a str);

converters!(Content);

/// The target of the processing instruction (e.g. `xml` in `<?xml ?>`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Target<'a>(&'a str);

converters!(Target);

/// The content of the processing instruction (e.g. `encoding="utf8"` in `<?xml
/// encoding="utf-8"?>`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Instructions<'a>(&'a str);

converters!(Instructions);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tag_name() {
        let tag_name = TagName::from_str("br");
        assert_eq!(tag_name.as_bytes(), b"br");
        assert_eq!(tag_name.as_str(), "br");
    }

    #[test]
    fn tag_name_with_namespace_prefix() {
        let tag_name = TagName::from_str("customer:id");
        assert_eq!(tag_name.local(), LocalName::from_str("id"));
        assert_eq!(tag_name.local().as_str(), "id");
        assert_eq!(
            tag_name.namespace_prefix(),
            Some(NamespacePrefix::from_str("customer"))
        );
        assert_eq!(tag_name.namespace_prefix().unwrap().as_str(), "customer");
    }

    #[test]
    fn tag_name_without_namespace_prefix() {
        let tag_name = TagName::from_str("id");
        assert_eq!(tag_name.local(), LocalName::from_str("id"));
        assert_eq!(tag_name.local().as_str(), "id");
        assert_eq!(tag_name.namespace_prefix(), None);
    }

    #[test]
    fn tag_name_empty() {
        let tag_name = TagName::from_str("");
        assert_eq!(tag_name.as_str(), "");
        assert_eq!(tag_name.as_bytes(), b"");
        assert_eq!(tag_name.local().as_str(), "");
        assert_eq!(tag_name.local().as_bytes(), b"");
        assert_eq!(tag_name.namespace_prefix(), None);

        let tag_name = TagName::from_str(" ");
        assert_eq!(tag_name.as_str(), " ");
        assert_eq!(tag_name.as_bytes(), b" ");
        assert_eq!(tag_name.local().as_str(), " ");
        assert_eq!(tag_name.local().as_bytes(), b" ");
        assert_eq!(tag_name.namespace_prefix(), None);

        let tag_name = TagName::from_str("   ");
        assert_eq!(tag_name.as_str(), "   ");
        assert_eq!(tag_name.as_bytes(), b"   ");
        assert_eq!(tag_name.local().as_str(), "   ");
        assert_eq!(tag_name.local().as_bytes(), b"   ");
        assert_eq!(tag_name.namespace_prefix(), None);
    }

    #[test]
    fn attribute_standalone_space() {
        let attr = Attribute::from_str("attr");
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(None, attr.value());

        let attr = Attribute::from_str(" attr ");
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(None, attr.value());

        let attr = Attribute::from_str("  attr  ");
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(None, attr.value());
    }

    #[test]
    fn attribute_no_quotes_space() {
        let attr = Attribute::from_str("attr=test");
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("test"), attr.value().map(|a| a.as_str()));

        let attr = Attribute::from_str(" attr = test");
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("test"), attr.value().map(|a| a.as_str()));

        let attr = Attribute::from_str("  attr  =  test");
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("test"), attr.value().map(|a| a.as_str()));
    }

    #[test]
    fn attribute_single_quotes_space() {
        let attr = Attribute::from_str("attr='test'");
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("test"), attr.value().map(|a| a.as_str()));

        let attr = Attribute::from_str(" attr = 'test'");
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("test"), attr.value().map(|a| a.as_str()));

        let attr = Attribute::from_str("  attr  =  'test'");
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("test"), attr.value().map(|a| a.as_str()));

        let attr = Attribute::from_str("  attr  =  ' test '");
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some(" test "), attr.value().map(|a| a.as_str()));

        let attr = Attribute::from_str("  attr  =  '  test  '");
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("  test  "), attr.value().map(|a| a.as_str()));
    }

    #[test]
    fn attribute_double_quotes_space() {
        let attr = Attribute::from_str(r#"attr="test""#);
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("test"), attr.value().map(|a| a.as_str()));

        let attr = Attribute::from_str(r#" attr = "test""#);
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("test"), attr.value().map(|a| a.as_str()));

        let attr = Attribute::from_str(r#"  attr  =  "test""#);
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("test"), attr.value().map(|a| a.as_str()));

        let attr = Attribute::from_str(r#"  attr  =  " test ""#);
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some(" test "), attr.value().map(|a| a.as_str()));

        let attr = Attribute::from_str(r#"  attr  =  "  test  ""#);
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("  test  "), attr.value().map(|a| a.as_str()));
    }

    #[test]
    fn attribute_empty() {
        let att = Attribute::from_str("");
        assert_eq!(AttributeName::from_str(""), att.name());
        assert_eq!(None, att.value());

        let att = Attribute::from_str(" ");
        assert_eq!(AttributeName::from_str(""), att.name());
        assert_eq!(None, att.value());

        let att = Attribute::from_str("   ");
        assert_eq!(AttributeName::from_str(""), att.name());
        assert_eq!(None, att.value());
    }

    #[test]
    fn attribute_name_empty() {
        let att_name = AttributeName::from_str("");
        assert_eq!(att_name.as_str(), "");
        assert_eq!(att_name.as_bytes(), b"");
        assert_eq!(att_name.local(), LocalName::from_str(""));
        assert_eq!(att_name.namespace_prefix(), None);

        let att_name = AttributeName::from_str(" ");
        assert_eq!(att_name.as_str(), " ");
        assert_eq!(att_name.as_bytes(), b" ");
        assert_eq!(att_name.local(), LocalName::from_str(" "));
        assert_eq!(att_name.namespace_prefix(), None);

        let att_name = AttributeName::from_str("   ");
        assert_eq!(att_name.as_str(), "   ");
        assert_eq!(att_name.as_bytes(), b"   ");
        assert_eq!(att_name.local(), LocalName::from_str("   "));
        assert_eq!(att_name.namespace_prefix(), None);
    }

    #[test]
    fn attribute_value_empty() {
        let att_value = AttributeValue::from_str("");
        assert_eq!(att_value.as_str(), "");
        assert_eq!(att_value.as_bytes(), b"");

        let att_value = AttributeValue::from_str(" ");
        assert_eq!(att_value.as_str(), " ");
        assert_eq!(att_value.as_bytes(), b" ");

        let att_value = AttributeValue::from_str("   ");
        assert_eq!(att_value.as_str(), "   ");
        assert_eq!(att_value.as_bytes(), b"   ");
    }

    #[test]
    fn empty_attributes() {
        let attributes = Attributes::from_str("");
        assert_eq!(attributes.parse(0), None);
        assert_eq!(attributes.into_iter().next(), None);

        let attributes = Attributes::from_str(" ");
        assert_eq!(attributes.parse(0), None);
        assert_eq!(attributes.into_iter().next(), None);

        let attributes = Attributes::from_str("   ");
        assert_eq!(attributes.parse(0), None);
        assert_eq!(attributes.into_iter().next(), None);
    }

    #[test]
    fn attributes_single() {
        let attributes = Attributes::from_str(" attr=\"1\"");
        let mut attributes_into_iter = attributes.into_iter();
        let attr = attributes_into_iter.next().unwrap();
        assert_eq!(Attribute::from_str(" attr=\"1\""), attr);
        assert_eq!("attr", attr.name().as_str());
        assert_eq!("1", attr.value().unwrap().as_str());
        assert_eq!(attributes_into_iter.next(), None);
    }

    #[test]
    fn attributes_single_with_spaces() {
        let attributes = Attributes::from_str("   attr=\"1 example\" ");
        let mut attributes_into_iter = attributes.into_iter();

        let attr = attributes_into_iter.next().unwrap();
        assert_eq!(Attribute::from_str("   attr=\"1 example\""), attr);
        assert_eq!("attr", attr.name().as_str());
        assert_eq!("1 example", attr.value().unwrap().as_str());
        assert_eq!(attributes_into_iter.next(), None);
    }

    #[test]
    fn attributes_multiple() {
        let attributes =
            Attributes::from_str(" attr=\"1\" id='test' name=invalid name=\"multiple example\"");
        let mut attributes_into_iter = attributes.into_iter();

        let attr = attributes_into_iter.next().unwrap();
        assert_eq!(Attribute::from_str(" attr=\"1\""), attr);
        assert_eq!("attr", attr.name().as_str());
        assert_eq!("1", attr.value().unwrap().as_str());

        let attr = attributes_into_iter.next().unwrap();
        assert_eq!(Attribute::from_str(" id='test'"), attr);
        assert_eq!("id", attr.name().as_str());
        assert_eq!("test", attr.value().unwrap().as_str());

        let attr = attributes_into_iter.next().unwrap();
        assert_eq!(Attribute::from_str(" name=invalid"), attr);
        assert_eq!("name", attr.name().as_str());
        assert_eq!("invalid", attr.value().unwrap().as_str());

        let attr = attributes_into_iter.next().unwrap();
        assert_eq!(Attribute::from_str(" name=\"multiple example\""), attr);
        assert_eq!("name", attr.name().as_str());
        assert_eq!("multiple example", attr.value().unwrap().as_str());

        assert_eq!(attributes_into_iter.next(), None);
    }

    #[test]
    fn attributes_multiple_with_spaces() {
        let attributes = Attributes::from_str(
            "     attr=\"1\"  id ='test' test  =  new   name= invalid standalone   name=\"example\"  ",
        );

        let mut attributes_into_iter = attributes.into_iter();

        let attr = attributes_into_iter.next().unwrap();
        assert_eq!(Attribute::from_str("     attr=\"1\""), attr);
        assert_eq!("attr", attr.name().as_str());
        assert_eq!("1", attr.value().unwrap().as_str());

        let attr = attributes_into_iter.next().unwrap();
        assert_eq!(Attribute::from_str("  id ='test'"), attr);
        assert_eq!("id", attr.name().as_str());
        assert_eq!("test", attr.value().unwrap().as_str());

        let attr = attributes_into_iter.next().unwrap();
        assert_eq!(Attribute::from_str(" test  =  new"), attr);
        assert_eq!("test", attr.name().as_str());
        assert_eq!("new", attr.value().unwrap().as_str());

        let attr = attributes_into_iter.next().unwrap();
        assert_eq!(Attribute::from_str("   name= invalid"), attr);
        assert_eq!("name", attr.name().as_str());
        assert_eq!("invalid", attr.value().unwrap().as_str());

        let attr = attributes_into_iter.next().unwrap();
        assert_eq!(Attribute::from_str(" standalone"), attr);
        assert_eq!("standalone", attr.name().as_str());
        assert_eq!(None, attr.value());

        let attr = attributes_into_iter.next().unwrap();
        assert_eq!(Attribute::from_str("   name=\"example\""), attr);
        assert_eq!("name", attr.name().as_str());
        assert_eq!("example", attr.value().unwrap().as_str());

        assert_eq!(attributes_into_iter.next(), None);
    }

    #[test]
    fn attributes_parse_multiple_with_spaces() {
        let attributes = Attributes::from_str(
            "     attr=\"1\"  id='test' test = new   name= invalid standalone   name=\"example\" ",
        );

        let mut pos = 0;

        let attr = attributes.parse(pos).unwrap();
        assert_eq!(Attribute::from_str("     attr=\"1\""), attr);
        assert_eq!("attr", attr.name().as_str());
        assert_eq!("1", attr.value().unwrap().as_str());
        pos += attr.len();

        let attr = attributes.parse(pos).unwrap();
        assert_eq!(Attribute::from_str("  id='test'"), attr);
        assert_eq!("id", attr.name().as_str());
        assert_eq!("test", attr.value().unwrap().as_str());
        pos += attr.len();

        let attr = attributes.parse(pos).unwrap();
        assert_eq!(Attribute::from_str(" test = new"), attr);
        assert_eq!("test", attr.name().as_str());
        assert_eq!("new", attr.value().unwrap().as_str());
        pos += attr.len();

        let attr = attributes.parse(pos).unwrap();
        assert_eq!(Attribute::from_str("   name= invalid"), attr);
        assert_eq!("name", attr.name().as_str());
        assert_eq!("invalid", attr.value().unwrap().as_str());
        pos += attr.len();

        let attr = attributes.parse(pos).unwrap();
        assert_eq!(Attribute::from_str(" standalone"), attr);
        assert_eq!("standalone", attr.name().as_str());
        assert_eq!(None, attr.value());
        pos += attr.len();

        let attr = attributes.parse(pos).unwrap();
        assert_eq!(Attribute::from_str("   name=\"example\""), attr);
        assert_eq!("name", attr.name().as_str());
        assert_eq!("example", attr.value().unwrap().as_str());
        pos += attr.len();

        assert_eq!(None, attributes.parse(pos));
    }

    #[test]
    fn attribute_name_and_value() {
        let attribute = Attribute::from_str("attr=\"1\"");
        assert_eq!(attribute.name(), AttributeName::from_str("attr"));
        assert_eq!(attribute.value(), Some(AttributeValue::from_str("1")));

        let attribute = Attribute::from_str("id='test'");
        assert_eq!(attribute.name(), AttributeName::from_str("id"));
        assert_eq!(attribute.value(), Some(AttributeValue::from_str("test")));

        let attribute = Attribute::from_str("test =new");
        assert_eq!(attribute.name(), AttributeName::from_str("test"));
        assert_eq!(attribute.value(), Some(AttributeValue::from_str("new")));

        let attribute = Attribute::from_str("name= invalid");
        assert_eq!(attribute.name(), AttributeName::from_str("name"));
        assert_eq!(attribute.value(), Some(AttributeValue::from_str("invalid")));

        let attribute = Attribute::from_str("standalone");
        assert_eq!(attribute.name(), AttributeName::from_str("standalone"));
        assert_eq!(attribute.value(), None);

        let attribute = Attribute::from_str("xml:example=\"test\"");
        assert_eq!(attribute.name(), AttributeName::from_str("xml:example"));
        assert_eq!(attribute.value(), Some(AttributeValue::from_str("test")));
    }
}
