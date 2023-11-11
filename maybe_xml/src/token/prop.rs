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
            /// Instantiates a new instance with a string.
            #[inline]
            #[must_use]
            pub const fn from_str(value: &'a str) -> Self {
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

        impl<'a> From<&'a str> for $name<'a> {
            fn from(value: &'a str) -> Self {
                Self(value)
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
    /// For example, if `xml:example` was the tag name, then `example` would be the local part of the name.
    /// If there is no namespace prefix, the entire name is returned.
    #[must_use]
    pub fn local(&self) -> LocalName<'a> {
        if let Some(index) = self
            .0
            .char_indices()
            .find_map(|(pos, ch)| (ch == ':').then(|| pos))
        {
            LocalName(&self.0[index + ':'.len_utf8()..])
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
            .char_indices()
            .find_map(|(pos, ch)| (ch == ':').then(|| NamespacePrefix(&self.0[..pos])))
    }
}

converters!(TagName);

/// The local part of the name.
///
/// For example, if `xml:example` was the tag name, then `example` would be the local part of the name.
/// If there is no namespace prefix, the entire name is returned.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalName<'a>(&'a str);

converters!(LocalName);

/// The namespace prefix if available.
///
/// For example if `xml:namespace` was the tag name, then `xml` would be the namespace prefix.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NamespacePrefix<'a>(&'a str);

converters!(NamespacePrefix);

/// All of the attribute bytes of a tag.
///
/// For the vast majority of use cases, a library user should call `iter()` or `.into_iter()`.
///
/// The bytes may include additional spacing in the raw byte form.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Attributes<'a>(&'a str);

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

converters!(Attributes);

#[must_use]
fn iter_attr(mut index: usize, value: &str) -> (usize, Option<Attribute<'_>>) {
    if index == value.len() {
        return (index, None);
    }

    if let Some(begin) = value[index..]
        .char_indices()
        .find_map(|(pos, ch)| (!super::is_space_ch(ch)).then(|| pos))
    {
        let mut saw_space_before_equals = false;
        let mut last_nonspace_index_before_equals = None;
        let mut saw_equals = false;
        let mut saw_characters_after_equals = false;
        let mut quote_state = QuoteState::None;
        for (loop_index, ch) in value[index + begin..].char_indices() {
            match ch {
                '"' => match quote_state {
                    QuoteState::None => quote_state = QuoteState::Double,
                    QuoteState::Single => {}
                    QuoteState::Double => {
                        if saw_equals {
                            let attr = Attribute(
                                &value[(index + begin)
                                    ..(index + begin + loop_index + '"'.len_utf8())],
                            );
                            index += begin + loop_index + '"'.len_utf8();
                            return (index, Some(attr));
                        }
                    }
                },
                '\'' => match quote_state {
                    QuoteState::None => quote_state = QuoteState::Single,
                    QuoteState::Single => {
                        if saw_equals {
                            let attr = Attribute(
                                &value[(index + begin)
                                    ..(index + begin + loop_index + '\''.len_utf8())],
                            );
                            index += begin + loop_index + '\''.len_utf8();
                            return (index, Some(attr));
                        }
                    }
                    QuoteState::Double => {}
                },
                '=' => saw_equals = true,
                ch if super::is_space_ch(ch) => {
                    if !saw_equals {
                        saw_space_before_equals = true;
                    } else if saw_characters_after_equals {
                        match quote_state {
                            QuoteState::None => {
                                let attr =
                                    Attribute(&value[index + begin..index + begin + loop_index]);
                                index += begin + loop_index + ch.len_utf8();
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
                                &value[(index + begin)..(index + begin + last_seen_char)],
                            );
                            index += begin + loop_index;
                            return (index, Some(attr));
                        }

                        let attr = Attribute(&value[index + begin..index + begin + loop_index]);
                        index += begin + loop_index;
                        return (index, Some(attr));
                    } else {
                        last_nonspace_index_before_equals = Some(loop_index + ch.len_utf8());
                    }
                }
            }
        }
        let attr = Attribute(&value[index + begin..]);
        index = value.len();
        (index, Some(attr))
    } else {
        index = value.len();
        (index, None)
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
        let (new_index, item) = iter_attr(self.index, self.value);
        self.index = new_index;
        item
    }
}

/// The name and the optional associated value of an attribute.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Attribute<'a>(&'a str);

impl<'a> Attribute<'a> {
    /// The attribute's name.
    #[must_use]
    pub fn name(&self) -> AttributeName<'a> {
        if let Some(index) = self
            .0
            .char_indices()
            .find_map(|(pos, ch)| (ch == '=').then(|| pos))
        {
            if let Some((pos, ch)) = self.0[..index]
                .char_indices()
                .rev()
                .find(|(_, ch)| !super::is_space_ch(*ch))
            {
                let end = pos + ch.len_utf8();
                return AttributeName(&self.0[..end]);
            }
        }
        AttributeName(self.0)
    }

    /// The optional attribute value with the quotes removed.
    #[must_use]
    pub fn value(&self) -> Option<AttributeValue<'a>> {
        if let Some(index) = self
            .0
            .char_indices()
            .find_map(|(pos, ch)| (ch == '=').then(|| pos))
        {
            let after_equal_index = index + '='.len_utf8();
            let mut quote_state = QuoteState::None;
            let mut begin = after_equal_index;
            let mut first_nonspace = None;
            let mut last_nonspace = after_equal_index;
            for (loop_index, ch) in self.0[after_equal_index..].char_indices() {
                match ch {
                    '"' => match quote_state {
                        QuoteState::None => {
                            begin = loop_index + '"'.len_utf8();
                            quote_state = QuoteState::Double;
                        }
                        QuoteState::Single => {}
                        QuoteState::Double => {
                            return Some(AttributeValue(
                                &self.0[after_equal_index + begin..after_equal_index + loop_index],
                            ));
                        }
                    },
                    '\'' => match quote_state {
                        QuoteState::None => {
                            begin = loop_index + '\''.len_utf8();
                            quote_state = QuoteState::Single;
                        }
                        QuoteState::Single => {
                            return Some(AttributeValue(
                                &self.0[after_equal_index + begin..after_equal_index + loop_index],
                            ));
                        }
                        QuoteState::Double => {}
                    },
                    ch => {
                        if !super::is_space_ch(ch) {
                            last_nonspace = loop_index + ch.len_utf8();
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
                AttributeValue(
                    &self.0[after_equal_index + begin..after_equal_index + last_nonspace],
                )
            })
        } else {
            None
        }
    }
}

converters!(Attribute);

/// An attribute's name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AttributeName<'a>(&'a str);

impl<'a> AttributeName<'a> {
    /// The local part of the name.
    ///
    /// For example, if `xml:example` was the attribute name, then `example` would be the local part of the name.
    /// If there is no namespace prefix, the entire name is returned.
    #[must_use]
    pub fn local(&self) -> LocalName<'a> {
        if let Some(index) = self
            .0
            .char_indices()
            .find_map(|(pos, ch)| (ch == ':').then(|| pos))
        {
            LocalName(&self.0[index + ':'.len_utf8()..])
        } else {
            LocalName(self.0)
        }
    }

    /// The namespace prefix if available.
    ///
    /// For example if `xml:example` was the attribute name, then `xml` would be the namespace prefix.
    #[must_use]
    pub fn namespace_prefix(&self) -> Option<NamespacePrefix<'a>> {
        self.0
            .char_indices()
            .find_map(|(pos, ch)| (ch == ':').then(|| NamespacePrefix(&self.0[..pos])))
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

/// The content of the processing instruction (e.g. `encoding="utf8"` in `<?xml encoding="utf-8"?>`).
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
    fn empty_attributes() {
        let attributes = Attributes::from_str("");
        assert_eq!(attributes.into_iter().next(), None);
    }

    #[test]
    fn attributes_single() {
        let attributes = Attributes::from_str("attr=\"1\"");
        let mut attributes_iter = attributes.into_iter();
        assert_eq!(
            attributes_iter.next(),
            Some(Attribute::from_str("attr=\"1\""))
        );
        assert_eq!(attributes_iter.next(), None);

        let mut attributes_into_iter = attributes.into_iter();
        assert_eq!(
            attributes_into_iter.next(),
            Some(Attribute::from_str("attr=\"1\""))
        );
        assert_eq!(attributes_into_iter.next(), None);
    }

    #[test]
    fn attributes_single_with_spaces() {
        let attributes = Attributes::from_str("   attr=\"1 example\" ");
        let mut attributes_iter = attributes.into_iter();
        assert_eq!(
            attributes_iter.next(),
            Some(Attribute::from_str("attr=\"1 example\""))
        );
        assert_eq!(attributes_iter.next(), None);

        let mut attributes_into_iter = attributes.into_iter();
        assert_eq!(
            attributes_into_iter.next(),
            Some(Attribute::from_str("attr=\"1 example\""))
        );
        assert_eq!(attributes_into_iter.next(), None);
    }

    #[test]
    fn attributes_multiple() {
        let attributes =
            Attributes::from_str("attr=\"1\" id='test' name=invalid name=\"multiple example\"");
        let mut attributes_iter = attributes.into_iter();
        assert_eq!(
            attributes_iter.next(),
            Some(Attribute::from_str("attr=\"1\""))
        );
        assert_eq!(
            attributes_iter.next(),
            Some(Attribute::from_str("id='test'"))
        );
        assert_eq!(
            attributes_iter.next(),
            Some(Attribute::from_str("name=invalid"))
        );
        assert_eq!(
            attributes_iter.next(),
            Some(Attribute::from_str("name=\"multiple example\""))
        );
        assert_eq!(attributes_iter.next(), None);

        let mut attributes_into_iter = attributes.into_iter();
        assert_eq!(
            attributes_into_iter.next(),
            Some(Attribute::from_str("attr=\"1\""))
        );
        assert_eq!(
            attributes_into_iter.next(),
            Some(Attribute::from_str("id='test'"))
        );
        assert_eq!(
            attributes_into_iter.next(),
            Some(Attribute::from_str("name=invalid"))
        );
        assert_eq!(
            attributes_into_iter.next(),
            Some(Attribute::from_str("name=\"multiple example\""))
        );
        assert_eq!(attributes_into_iter.next(), None);
    }

    #[test]
    fn attributes_multiple_with_spaces() {
        let attributes = Attributes::from_str(
            "     attr=\"1\"  id='test' test = new   name= invalid standalone   name=\"example\" ",
        );

        let mut attributes_into_iter = attributes.into_iter();
        assert_eq!(
            attributes_into_iter.next(),
            Some(Attribute::from_str("attr=\"1\""))
        );
        assert_eq!(
            attributes_into_iter.next(),
            Some(Attribute::from_str("id='test'"))
        );
        assert_eq!(
            attributes_into_iter.next(),
            Some(Attribute::from_str("test = new"))
        );
        assert_eq!(
            attributes_into_iter.next(),
            Some(Attribute::from_str("name= invalid"))
        );
        assert_eq!(
            attributes_into_iter.next(),
            Some(Attribute::from_str("standalone"))
        );
        assert_eq!(
            attributes_into_iter.next(),
            Some(Attribute::from_str("name=\"example\""))
        );
        assert_eq!(attributes_into_iter.next(), None);
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
