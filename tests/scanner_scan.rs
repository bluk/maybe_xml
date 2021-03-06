// Copyright 2020 Bryant Luk
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use maybe_xml::scanner::{Scanner, State};

const SIMPLE_1_XML_BYTES: &[u8] = include_bytes!("../tests/resources/simple-1.xml");
const SVG_1_XML_BYTES: &[u8] = include_bytes!("../tests/resources/svg-1.xml");
const LARGE_1_XML_BYTES: &[u8] = include_bytes!("../tests/resources/large-1.xml");

fn scanner_scan(bytes: &[u8], expected_states: &[State]) {
    let mut bytes = bytes;
    let mut scanner = Scanner::new();
    let mut expected_state_iter = expected_states.iter();

    while let Some(state) = scanner.scan(&bytes) {
        assert_eq!(Some(state).as_ref(), expected_state_iter.next());

        match state {
            State::ScanningMarkup => {
                bytes = &bytes[bytes.len()..];
            }
            State::ScanningStartOrEmptyElementTag => {
                bytes = &bytes[bytes.len()..];
            }
            State::ScanningCharacters => {
                bytes = &bytes[bytes.len()..];
            }
            State::ScanningEndTag => {
                bytes = &bytes[bytes.len()..];
            }
            State::ScanningProcessingInstruction => {
                bytes = &bytes[bytes.len()..];
            }
            State::ScanningDeclarationCommentOrCdata => {
                bytes = &bytes[bytes.len()..];
            }
            State::ScanningDeclaration => {
                bytes = &bytes[bytes.len()..];
            }
            State::ScanningCdata => {
                bytes = &bytes[bytes.len()..];
            }
            State::ScanningComment => {
                bytes = &bytes[bytes.len()..];
            }
            State::ScannedEmptyElementTag(read) => {
                bytes = &bytes[read..];
            }
            State::ScannedEndTag(read) => {
                bytes = &bytes[read..];
            }
            State::ScannedProcessingInstruction(read) => {
                bytes = &bytes[read..];
            }
            State::ScannedStartTag(read) => {
                bytes = &bytes[read..];
            }
            State::ScannedCharacters(read) => {
                bytes = &bytes[read..];
            }
            State::ScannedDeclaration(read) => {
                bytes = &bytes[read..];
            }
            State::ScannedComment(read) => {
                bytes = &bytes[read..];
            }
            State::ScannedCdata(read) => {
                bytes = &bytes[read..];
            }
        }
    }
    assert_eq!(expected_state_iter.next(), None);
}

fn scanner_scan_2(bytes: &[u8]) {
    let mut bytes = bytes;
    let mut scanner = Scanner::new();

    while let Some(state) = scanner.scan(&bytes) {
        println!("{:?} - {}", state, bytes.len());

        match state {
            State::ScanningMarkup => {
                bytes = &bytes[bytes.len()..];
            }
            State::ScanningStartOrEmptyElementTag => {
                bytes = &bytes[bytes.len()..];
            }
            State::ScanningCharacters => {
                bytes = &bytes[bytes.len()..];
            }
            State::ScanningEndTag => {
                bytes = &bytes[bytes.len()..];
            }
            State::ScanningProcessingInstruction => {
                bytes = &bytes[bytes.len()..];
            }
            State::ScanningDeclarationCommentOrCdata => {
                bytes = &bytes[bytes.len()..];
            }
            State::ScanningDeclaration => {
                bytes = &bytes[bytes.len()..];
            }
            State::ScanningCdata => {
                bytes = &bytes[bytes.len()..];
            }
            State::ScanningComment => {
                bytes = &bytes[bytes.len()..];
            }
            State::ScannedEmptyElementTag(read) => {
                bytes = &bytes[read..];
            }
            State::ScannedEndTag(read) => {
                bytes = &bytes[read..];
            }
            State::ScannedProcessingInstruction(read) => {
                bytes = &bytes[read..];
            }
            State::ScannedStartTag(read) => {
                bytes = &bytes[read..];
            }
            State::ScannedCharacters(read) => {
                bytes = &bytes[read..];
            }
            State::ScannedDeclaration(read) => {
                bytes = &bytes[read..];
            }
            State::ScannedComment(read) => {
                bytes = &bytes[read..];
            }
            State::ScannedCdata(read) => {
                bytes = &bytes[read..];
            }
        }
    }
}

#[test]
fn scanner_scan_simple_1_xml() {
    scanner_scan(
        SIMPLE_1_XML_BYTES,
        &[
            State::ScannedProcessingInstruction(21),
            State::ScannedCharacters(1),
            State::ScannedStartTag(10),
            State::ScannedCharacters(12),
            State::ScannedEndTag(11),
        ],
    );
}

#[test]
fn scanner_scan_svg_1_xml() {
    scanner_scan(
        SVG_1_XML_BYTES,
        &[
            State::ScannedProcessingInstruction(21),
            State::ScannedCharacters(1),
            State::ScannedDeclaration(100),
            State::ScannedCharacters(2),
            State::ScannedStartTag(70),
            State::ScannedCharacters(3),
            State::ScannedEmptyElementTag(84),
            State::ScannedCharacters(1),
            State::ScannedEndTag(6),
        ],
    );
}

#[test]
fn scanner_scan_large_1_xml() {
    scanner_scan_2(LARGE_1_XML_BYTES);
}
