// Copyright 2022 Bryant Luk
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use criterion::{criterion_group, criterion_main, Criterion};

const SIMPLE_1_BYTES: &[u8] = include_bytes!("../../maybe_xml/tests/resources/simple-1.xml");
const SVG_1_BYTES: &[u8] = include_bytes!("../../maybe_xml/tests/resources/svg-1.xml");
const RSS_1_BYTES: &[u8] = include_bytes!("../../maybe_xml/tests/resources/rss-1.xml");
const LARGE_1_BYTES: &[u8] = include_bytes!("../../maybe_xml/tests/resources/large-1.xml");

fn scanner_scan(bytes: &[u8]) -> usize {
    use maybe_xml::scanner::{Scanner, State};

    let mut count = 0;

    let mut bytes = bytes;
    let mut scanner = Scanner::new();
    while let Some(state) = scanner.scan(bytes) {
        match state {
            State::ScanningMarkup
            | State::ScanningStartOrEmptyElementTag
            | State::ScanningCharacters
            | State::ScanningEndTag
            | State::ScanningProcessingInstruction
            | State::ScanningDeclarationCommentOrCdata
            | State::ScanningDeclaration
            | State::ScanningComment
            | State::ScanningCdata => {
                bytes = &bytes[bytes.len()..];
            }
            State::ScannedEmptyElementTag(read)
            | State::ScannedEndTag(read)
            | State::ScannedProcessingInstruction(read)
            | State::ScannedCharacters(read)
            | State::ScannedDeclaration(read)
            | State::ScannedComment(read)
            | State::ScannedCdata(read) => {
                bytes = &bytes[read..];
            }
            State::ScannedStartTag(read) => {
                bytes = &bytes[read..];
                count += 1;
            }
        }
    }

    count
}

fn lexer_into_iter(bytes: &[u8]) -> u64 {
    use maybe_xml::{token::Ty, Lexer};

    let lexer = Lexer::from_slice(bytes);
    let mut count = 0;

    for token in lexer {
        match token.ty() {
            Ty::StartTag(_) => count += 1,
            Ty::EmptyElementTag(_)
            | Ty::EndTag(_)
            | Ty::Characters(_)
            | Ty::ProcessingInstruction(_)
            | Ty::Declaration(_)
            | Ty::Comment(_)
            | Ty::Cdata(_) => {}
        }
    }

    count
}

#[allow(clippy::too_many_lines)]
fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("parser_parse_simple_xml_1", |b| {
        b.iter(|| {
            let count = scanner_scan(SIMPLE_1_BYTES);
            assert_eq!(1, count);
        });
    });
    c.bench_function("parser_parse_svg_1", |b| {
        b.iter(|| {
            let count = scanner_scan(SVG_1_BYTES);
            assert_eq!(1, count);
        });
    });
    c.bench_function("parser_parse_rss_1", |b| {
        b.iter(|| {
            let count = scanner_scan(RSS_1_BYTES);
            assert_eq!(36, count);
        });
    });
    c.bench_function("scanner_scan_large_1", |b| {
        b.iter(|| {
            let count = scanner_scan(LARGE_1_BYTES);
            assert_eq!(9885, count);
        });
    });

    c.bench_function("lexer_into_iter_simple_xml_1", |b| {
        b.iter(|| {
            let count = lexer_into_iter(SIMPLE_1_BYTES);
            assert_eq!(1, count);
        });
    });
    c.bench_function("lexer_into_iter_svg_1", |b| {
        b.iter(|| {
            let count = lexer_into_iter(SVG_1_BYTES);
            assert_eq!(1, count);
        });
    });
    c.bench_function("lexer_into_iter_rss_1", |b| {
        b.iter(|| {
            let count = lexer_into_iter(RSS_1_BYTES);
            assert_eq!(36, count);
        });
    });
    c.bench_function("lexer_into_iter_large_1", |b| {
        b.iter(|| {
            let count = lexer_into_iter(LARGE_1_BYTES);
            assert_eq!(count, 9885);
        });
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
