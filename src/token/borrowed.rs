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

pub use super::{
    Cdata, Characters, Comment, Declaration, EmptyElementTag, EndTag, ProcessingInstruction,
    StartTag,
};

/// Scanned byte values associated with a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

/// Bytes not evaluated when an end of file is reached.
///
/// If a markup starting delimiter (e.g. a `<`) was read but there was no closing
/// delimiter (e.g. `>`) read and the end of file marker is reached, then the
/// leftover bytes are enclosed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BytesNotEvaluated<'a>(&'a [u8]);

use super::converters;

converters!(BytesNotEvaluated);
