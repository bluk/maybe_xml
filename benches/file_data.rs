// Copyright 2022 Bryant Luk
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use criterion::{criterion_group, criterion_main, Criterion};

const SIMPLE_1_BYTES: &[u8] = include_bytes!("../tests/resources/simple-1.xml");
const SVG_1_BYTES: &[u8] = include_bytes!("../tests/resources/svg-1.xml");
const RSS_1_BYTES: &[u8] = include_bytes!("../tests/resources/rss-1.xml");
const LARGE_1_BYTES: &[u8] = include_bytes!("../tests/resources/large-1.xml");

fn scanner_scan(bytes: &[u8]) {
    use maybe_xml::scanner::{Scanner, State};

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
            | State::ScannedStartTag(read)
            | State::ScannedCharacters(read)
            | State::ScannedDeclaration(read)
            | State::ScannedComment(read)
            | State::ScannedCdata(read) => {
                bytes = &bytes[read..];
            }
        }
    }
}

fn recv_buf_reader_recv_and_next_token(bytes: &[u8]) {
    use maybe_xml::{eval::recv::Evaluator, token::borrowed::Token};

    let mut eval = Evaluator::new();

    let mut bytes = bytes;

    loop {
        let read = eval.recv(bytes);
        bytes = &bytes[read..];
        if let Ok(token) = eval.next_token() {
            if let Some(token) = token {
                match token {
                    Token::StartTag(_start_tag) => {}
                    Token::EmptyElementTag(_empty_element_tag) => {}
                    Token::EndTag(_end_tag) => {}
                    Token::Characters(_characters) => {}
                    Token::ProcessingInstruction(_processing_instruction) => {}
                    Token::Declaration(_declaration) => {}
                    Token::Comment(_comment) => {}
                    Token::Cdata(_cdata) => {}
                    Token::Eof => {
                        break;
                    }
                    Token::EofWithBytesNotEvaluated(_bytes_not_evaluated) => break,
                }
            } else {
                break;
            }
        }
    }
}

fn io_buf_reader_next_token(bytes: &[u8]) {
    use maybe_xml::{eval::bufread::BufReadEvaluator, token::borrowed::Token};

    let mut eval = BufReadEvaluator::from_reader(bytes);

    loop {
        let token = eval.next_token().unwrap();
        if let Some(token) = token {
            match token {
                Token::StartTag(_start_tag) => {}
                Token::EmptyElementTag(_empty_element_tag) => {}
                Token::EndTag(_end_tag) => {}
                Token::Characters(_characters) => {}
                Token::ProcessingInstruction(_processing_instruction) => {}
                Token::Declaration(_declaration) => {}
                Token::Comment(_comment) => {}
                Token::Cdata(_cdata) => {}
                Token::Eof => {
                    break;
                }
                Token::EofWithBytesNotEvaluated(_bytes_not_evaluated) => {
                    break;
                }
            }
        } else {
            break;
        }
    }
}

fn io_buf_reader_into_iter_next(bytes: &[u8]) {
    use maybe_xml::{eval::bufread::BufReadEvaluator, token::owned::Token};

    let mut eval = BufReadEvaluator::from_reader(bytes).into_iter();

    loop {
        let token = eval.next();
        if let Some(token) = token {
            match token {
                Token::StartTag(_start_tag) => {}
                Token::EmptyElementTag(_empty_element_tag) => {}
                Token::EndTag(_end_tag) => {}
                Token::Characters(_characters) => {}
                Token::ProcessingInstruction(_processing_instruction) => {}
                Token::Declaration(_declaration) => {}
                Token::Comment(_comment) => {}
                Token::Cdata(_cdata) => {}
                Token::Eof => {
                    break;
                }
                Token::EofWithBytesNotEvaluated(_bytes_not_evaluated) => break,
            }
        } else {
            break;
        }
    }
}
fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("parser_parse_simple_xml_1", |b| {
        b.iter(|| scanner_scan(SIMPLE_1_BYTES));
    });
    c.bench_function("parser_parse_svg_1", |b| {
        b.iter(|| scanner_scan(SVG_1_BYTES));
    });
    c.bench_function("parser_parse_rss_1", |b| {
        b.iter(|| scanner_scan(RSS_1_BYTES));
    });
    c.bench_function("scanner_scan_large_1", |b| {
        b.iter(|| scanner_scan(LARGE_1_BYTES));
    });
    c.bench_function("recv_buf_reader_recv_and_next_event_simple_xml_1", |b| {
        b.iter(|| recv_buf_reader_recv_and_next_token(SIMPLE_1_BYTES));
    });
    c.bench_function("recv_buf_reader_recv_and_next_event_svg_1", |b| {
        b.iter(|| recv_buf_reader_recv_and_next_token(SVG_1_BYTES));
    });
    c.bench_function("recv_buf_reader_recv_and_next_event_rss_1", |b| {
        b.iter(|| recv_buf_reader_recv_and_next_token(RSS_1_BYTES));
    });
    c.bench_function("recv_evaluator_recv_and_next_token_large_1", |b| {
        b.iter(|| recv_buf_reader_recv_and_next_token(LARGE_1_BYTES));
    });
    c.bench_function("io_buf_reader_next_event_simple_xml_1", |b| {
        b.iter(|| io_buf_reader_next_token(SIMPLE_1_BYTES));
    });
    c.bench_function("io_buf_reader_next_event_svg_1", |b| {
        b.iter(|| io_buf_reader_next_token(SVG_1_BYTES));
    });
    c.bench_function("io_buf_reader_next_event_rss_1", |b| {
        b.iter(|| io_buf_reader_next_token(RSS_1_BYTES));
    });
    c.bench_function("buf_read_evaluator_next_token_large_1", |b| {
        b.iter(|| io_buf_reader_next_token(LARGE_1_BYTES));
    });
    c.bench_function("io_buf_reader_into_iter_next_simple_xml_1", |b| {
        b.iter(|| io_buf_reader_into_iter_next(SIMPLE_1_BYTES));
    });
    c.bench_function("io_buf_reader_into_iter_next_svg_1", |b| {
        b.iter(|| io_buf_reader_into_iter_next(SVG_1_BYTES));
    });
    c.bench_function("io_buf_reader_into_iter_next_rss_1", |b| {
        b.iter(|| io_buf_reader_into_iter_next(RSS_1_BYTES));
    });
    c.bench_function("buf_read_evaluator_into_iter_next_large_1", |b| {
        b.iter(|| io_buf_reader_into_iter_next(LARGE_1_BYTES));
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
