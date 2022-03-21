// Copyright 2022 Bryant Luk
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#[cfg(any(feature = "std", feature = "alloc"))]
use maybe_xml::{
    eval::recv::RecvEvaluator,
    token::borrowed::{
        Characters, Declaration, EmptyElementTag, EndTag, ProcessingInstruction, StartTag, Token,
    },
};

#[cfg(any(feature = "std", feature = "alloc"))]
const SIMPLE_1_XML_BYTES: &[u8] = include_bytes!("../tests/resources/simple-1.xml");
#[cfg(any(feature = "std", feature = "alloc"))]
const SVG_1_XML_BYTES: &[u8] = include_bytes!("../tests/resources/svg-1.xml");

#[cfg(any(feature = "std", feature = "alloc"))]
fn recv_eval(bytes: &[u8], expected_tokens: &[Token]) {
    let mut eval = RecvEvaluator::new();
    let mut expected_token_iter = expected_tokens.iter();

    let mut bytes = bytes;

    loop {
        let read = eval.recv(bytes);
        bytes = &bytes[read..];
        let token = eval.next_token().unwrap();
        assert_eq!(token.as_ref(), expected_token_iter.next());

        if let Some(token) = token {
            match token {
                Token::StartTag(_start_tag) => {}
                Token::EmptyElementTag(_empty_element_tag) => {}
                Token::EndTag(_end_tag) => {}
                Token::Characters(_text_content) => {}
                Token::ProcessingInstruction(_processing_instruction) => {}
                Token::Declaration(_declaration) => {}
                Token::Comment(_comment) => {}
                Token::Cdata(_cdata) => {}
                Token::Eof => {
                    break;
                }
                Token::EofWithBytesNotEvaluated(_bytes_not_evaluated) => {}
            }
        } else {
            break;
        }
    }
    assert_eq!(expected_token_iter.next(), None);
}

#[cfg(any(feature = "std", feature = "alloc"))]
#[test]
fn recv_eval_simple_1_xml() {
    recv_eval(
        SIMPLE_1_XML_BYTES,
        &[
            Token::ProcessingInstruction(ProcessingInstruction::from(
                r#"<?xml version="1.0"?>"#.as_bytes(),
            )),
            #[cfg(not(target_os = "windows"))]
            Token::Characters(Characters::from("\n".as_bytes())),
            #[cfg(target_os = "windows")]
            Token::Characters(Characters::from("\r\n".as_bytes())),
            Token::StartTag(StartTag::from(r#"<document>"#.as_bytes())),
            Token::Characters(Characters::from(r#"Hello world!"#.as_bytes())),
            Token::EndTag(EndTag::from(r#"</document>"#.as_bytes())),
            Token::Eof,
        ],
    );
}

#[cfg(any(feature = "std", feature = "alloc"))]
#[test]
fn recv_eval_svg_1_xml() {
    recv_eval(
        SVG_1_XML_BYTES,
        #[cfg(not(target_os = "windows"))]
        &[
            Token::ProcessingInstruction(ProcessingInstruction::from(
                r#"<?xml version="1.0"?>"#.as_bytes(),
            )),
            Token::Characters(Characters::from("\n".as_bytes())),
            Token::Declaration(Declaration::from("<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">".as_bytes())),
            Token::Characters(Characters::from("\n\n".as_bytes())),
            Token::StartTag(StartTag::from("<svg xmlns=\"http://www.w3.org/2000/svg\"\n     width=\"800\" height=\"800\">".as_bytes())),
            Token::Characters(Characters::from("\n  ".as_bytes())),
            Token::EmptyElementTag(EmptyElementTag::from("<circle cx=\"400\" cy=\"400\" r=\"50\" stroke=\"blue\"\n    stroke-width=\"1\" fill=\"yellow\" />".as_bytes())),
            Token::Characters(Characters::from("\n".as_bytes())),
            Token::EndTag(EndTag::from("</svg>".as_bytes())),
            Token::Eof,
        ],
        #[cfg(target_os = "windows")]
        &[
            Token::ProcessingInstruction(ProcessingInstruction::from(
                r#"<?xml version="1.0"?>"#.as_bytes(),
            )),
            Token::Characters(Characters::from("\r\n".as_bytes())),
            Token::Declaration(Declaration::from("<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\r\n  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">".as_bytes())),
            Token::Characters(Characters::from("\r\n\r\n".as_bytes())),
            Token::StartTag(StartTag::from("<svg xmlns=\"http://www.w3.org/2000/svg\"\r\n     width=\"800\" height=\"800\">".as_bytes())),
            Token::Characters(Characters::from("\r\n  ".as_bytes())),
            Token::EmptyElementTag(EmptyElementTag::from("<circle cx=\"400\" cy=\"400\" r=\"50\" stroke=\"blue\"\r\n    stroke-width=\"1\" fill=\"yellow\" />".as_bytes())),
            Token::Characters(Characters::from("\r\n".as_bytes())),
            Token::EndTag(EndTag::from("</svg>".as_bytes())),
            Token::Eof,
        ],
    );
}
