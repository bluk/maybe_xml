//! Properties are borrowed byte slice views into tokens.
//!
//! While tokens associate a byte sequence with a type such as a start tag or CDATA,
//! there may be interesting "properties" within the token such as the name of the
//! tag or the contents of a Characters/CDATA token.

use crate::QuoteState;

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

            /// Returns the length in bytes.
            #[allow(clippy::len_without_is_empty)]
            #[inline]
            #[must_use]
            pub const fn len(&self) -> usize {
                self.0.len()
            }

            /// The token property represented as a str.
            ///
            /// # Errors
            ///
            /// If the bytes are not a UTF-8 string.
            #[deprecated(since = "0.8.0", note = "Use as_str() instead.")]
            #[inline]
            pub fn to_str(&self) -> Result<&'a str, str::Utf8Error> {
                Ok(self.as_str())
            }

            /// The token property represented as a str.
            ///
            /// # Safety
            ///
            /// The underlying bytes are assumed to be UTF-8. If the bytes are
            /// not valid UTF-8, then the behavior is undefined.
            #[deprecated(since = "0.8.0", note = "Use as_str() instead.")]
            #[inline]
            #[must_use]
            pub const unsafe fn as_str_unchecked(&self) -> &'a str {
                self.as_str()
            }

            /// Returns the underlying slice.
            #[deprecated(since = "0.8.0", note = "Use as_bytes() instead.")]
            #[inline]
            #[must_use]
            pub const fn into_inner(self) -> &'a [u8] {
                self.as_bytes()
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
    #[rustversion::attr(since(1.71), const)]
    #[must_use]
    pub fn local(&self) -> LocalName<'a> {
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
    #[rustversion::attr(since(1.71), const)]
    #[must_use]
    pub fn namespace_prefix(&self) -> Option<NamespacePrefix<'a>> {
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
    /// let xml = r#"<id attr="1" id=test>"#;
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
    #[rustversion::attr(since(1.71), const)]
    #[inline]
    #[must_use]
    pub fn parse(&self, pos: usize) -> Option<Attribute<'a>> {
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

#[rustversion::attr(since(1.71), const)]
#[must_use]
fn iter_attr(index: usize, bytes: &[u8]) -> Option<usize> {
    if bytes.len() <= index {
        return None;
    }

    let mut begin = index;
    loop {
        if bytes.len() <= begin {
            return None;
        }

        let byte = bytes[begin];
        if !super::is_space(byte) {
            break;
        }

        begin += 1;
    }

    let byte = bytes[begin];

    let mut quote_state = match byte {
        b'"' => QuoteState::Double,
        b'\'' => QuoteState::Single,
        _ => QuoteState::None,
    };

    let mut saw_space_before_equals = false;
    let mut last_nonspace_index_before_equals = 0;
    let mut saw_equals = byte == b'=';
    let mut saw_characters_after_equals = false;

    let mut end = begin + 1;
    loop {
        if bytes.len() <= end {
            return Some(bytes.len());
        }

        match bytes[end] {
            b'"' => match quote_state {
                QuoteState::None => quote_state = QuoteState::Double,
                QuoteState::Single => {}
                QuoteState::Double => {
                    if saw_equals {
                        let end = end + '"'.len_utf8();
                        return Some(end);
                    }
                }
            },
            b'\'' => match quote_state {
                QuoteState::None => quote_state = QuoteState::Single,
                QuoteState::Single => {
                    if saw_equals {
                        let end = end + '\''.len_utf8();
                        return Some(end);
                    }
                }
                QuoteState::Double => {}
            },
            b'=' => saw_equals = true,
            byte if super::is_space(byte) => {
                if !saw_equals {
                    saw_space_before_equals = true;
                    // For the case of `name = value`, the space is
                    // acknowledged first only. If an `=` is seen after any
                    // spaces, the space is ignored. If an `=` is not seen
                    // after space, then return the name only (this is handled below).
                } else if saw_characters_after_equals {
                    match quote_state {
                        QuoteState::None => {
                            let end = end;
                            return Some(end);
                        }
                        QuoteState::Single | QuoteState::Double => {}
                    }
                }
            }
            _ => {
                if saw_equals {
                    saw_characters_after_equals = true;
                } else if saw_space_before_equals {
                    let end = last_nonspace_index_before_equals;
                    return Some(end);
                } else {
                    last_nonspace_index_before_equals = end + 1;
                }
            }
        }

        end += 1;
    }
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
    #[rustversion::attr(since(1.71), const)]
    #[must_use]
    pub fn name(&self) -> AttributeName<'a> {
        let bytes = self.0.as_bytes();

        let mut begin = 0;
        loop {
            if bytes.len() <= begin {
                return AttributeName::from_str(self.0);
            }

            let byte = bytes[begin];
            if !super::is_space(byte) {
                break;
            }

            begin += 1;
        }

        let mut index = begin;

        loop {
            if index == bytes.len() {
                let (_, bytes) = bytes.split_at(begin);
                let value = unsafe { core::str::from_utf8_unchecked(bytes) };
                return AttributeName::from_str(value);
            }

            let byte = bytes[index];
            if byte == b'=' {
                break;
            } else if super::is_space(byte) {
                let (bytes, _) = bytes.split_at(index);
                let (_, bytes) = bytes.split_at(begin);
                let value = unsafe { core::str::from_utf8_unchecked(bytes) };
                return AttributeName::from_str(value);
            }

            index += 1;
        }

        let (bytes, _) = bytes.split_at(index);

        let mut end = bytes.len() - 1;
        loop {
            if end == 0 {
                let (_, bytes) = bytes.split_at(begin);
                let value = unsafe { core::str::from_utf8_unchecked(bytes) };
                return AttributeName::from_str(value);
            }

            let byte = bytes[end];
            if !super::is_space(byte) {
                end += 1;
                break;
            }

            end -= 1;
        }

        let (bytes, _) = bytes.split_at(end);
        let (_, bytes) = bytes.split_at(begin);

        let value = unsafe { core::str::from_utf8_unchecked(bytes) };

        AttributeName::from_str(value)
    }

    /// The optional attribute value with the quotes removed.
    #[rustversion::attr(since(1.71), const)]
    #[must_use]
    pub fn value(&self) -> Option<AttributeValue<'a>> {
        let mut index = 0;
        let bytes = self.0.as_bytes();

        loop {
            if index == bytes.len() {
                return None;
            }

            let byte = bytes[index];
            if byte == b'=' {
                break;
            }
            index += 1;
        }

        let after_equal_index = index + '='.len_utf8();
        let mut quote_state = QuoteState::None;
        let mut begin = after_equal_index;
        let mut first_nonspace = None;
        let mut last_nonspace = after_equal_index;

        let mut loop_index = after_equal_index;
        loop {
            if loop_index == bytes.len() {
                break;
            }

            let byte = bytes[loop_index];
            match byte {
                b'"' => match quote_state {
                    QuoteState::None => {
                        begin = loop_index + '"'.len_utf8();
                        quote_state = QuoteState::Double;
                    }
                    QuoteState::Single => {}
                    QuoteState::Double => {
                        let (bytes, _) = bytes.split_at(loop_index);
                        let (_, bytes) = bytes.split_at(begin);
                        let value = unsafe { core::str::from_utf8_unchecked(bytes) };
                        return Some(AttributeValue::from_str(value));
                    }
                },
                b'\'' => match quote_state {
                    QuoteState::None => {
                        begin = loop_index + '\''.len_utf8();
                        quote_state = QuoteState::Single;
                    }
                    QuoteState::Single => {
                        let (bytes, _) = bytes.split_at(loop_index);
                        let (_, bytes) = bytes.split_at(begin);
                        let value = unsafe { core::str::from_utf8_unchecked(bytes) };
                        return Some(AttributeValue::from_str(value));
                    }
                    QuoteState::Double => {}
                },
                byte => {
                    if !super::is_space(byte) {
                        last_nonspace = loop_index + 1;
                        if first_nonspace.is_none() {
                            first_nonspace = Some(loop_index);
                        }
                    }
                }
            }

            loop_index += 1;
        }

        if let Some(begin) = first_nonspace {
            let (bytes, _) = bytes.split_at(last_nonspace);
            let (_, bytes) = bytes.split_at(begin);
            let value = unsafe { core::str::from_utf8_unchecked(bytes) };
            return Some(AttributeValue::from_str(value));
        }

        None
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
    #[rustversion::attr(since(1.71), const)]
    #[must_use]
    pub fn local(&self) -> LocalName<'a> {
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
    #[rustversion::attr(since(1.71), const)]
    #[must_use]
    pub fn namespace_prefix(&self) -> Option<NamespacePrefix<'a>> {
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

        let attr = Attribute::from_str(" attr = test ");
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("test"), attr.value().map(|a| a.as_str()));

        let attr = Attribute::from_str("  attr  =  test  ");
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("test"), attr.value().map(|a| a.as_str()));
    }

    #[test]
    fn attribute_single_quotes_space() {
        let attr = Attribute::from_str("attr='test'");
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("test"), attr.value().map(|a| a.as_str()));

        let attr = Attribute::from_str(" attr = 'test' ");
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("test"), attr.value().map(|a| a.as_str()));

        let attr = Attribute::from_str("  attr  =  'test'  ");
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("test"), attr.value().map(|a| a.as_str()));

        let attr = Attribute::from_str("  attr  =  ' test '  ");
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some(" test "), attr.value().map(|a| a.as_str()));

        let attr = Attribute::from_str("  attr  =  '  test  '  ");
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("  test  "), attr.value().map(|a| a.as_str()));
    }

    #[test]
    fn attribute_double_quotes_space() {
        let attr = Attribute::from_str(r#"attr="test""#);
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("test"), attr.value().map(|a| a.as_str()));

        let attr = Attribute::from_str(r#" attr = "test" "#);
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("test"), attr.value().map(|a| a.as_str()));

        let attr = Attribute::from_str(r#"  attr  =  "test"  "#);
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("test"), attr.value().map(|a| a.as_str()));

        let attr = Attribute::from_str(r#"  attr  =  " test "  "#);
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some(" test "), attr.value().map(|a| a.as_str()));

        let attr = Attribute::from_str(r#"  attr  =  "  test  "  "#);
        assert_eq!("attr", attr.name().as_str());
        assert_eq!(Some("  test  "), attr.value().map(|a| a.as_str()));
    }

    #[test]
    fn empty_attributes() {
        let attributes = Attributes::from_str("");
        assert_eq!(attributes.into_iter().next(), None);
    }

    #[test]
    fn attributes_single() {
        let attributes = Attributes::from_str("attr=\"1\"");
        let mut attributes_into_iter = attributes.into_iter();
        let attr = attributes_into_iter.next().unwrap();
        assert_eq!(Attribute::from_str("attr=\"1\""), attr);
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
            Attributes::from_str("attr=\"1\" id='test' name=invalid name=\"multiple example\"");
        let mut attributes_into_iter = attributes.into_iter();

        let attr = attributes_into_iter.next().unwrap();
        assert_eq!(Attribute::from_str("attr=\"1\""), attr);
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
