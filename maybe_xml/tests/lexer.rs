use maybe_xml::{
    token::{
        Characters, Declaration, EmptyElementTag, EndTag, ProcessingInstruction, StartTag, Ty,
    },
    Lexer,
};

const SIMPLE_1_XML_BYTES: &[u8] = include_bytes!("../tests/resources/simple-1.xml");
const SVG_1_XML_BYTES: &[u8] = include_bytes!("../tests/resources/svg-1.xml");

fn tokenize_via_iterator(bytes: &[u8], expected_tokens: &[Ty<'_>]) {
    let lexer = Lexer::from_slice(bytes);

    for (expected_token, token) in expected_tokens.iter().zip(lexer.iter(0)) {
        assert_eq!(*expected_token, token.ty());
    }

    assert_eq!(lexer.into_iter().count(), expected_tokens.len());
}

fn tokenize(bytes: &[u8], expected_tokens: &[Ty<'_>]) {
    let lexer = Lexer::from_slice(bytes);
    let mut pos = 0;

    let mut expected_iter = expected_tokens.iter().copied();

    while let Some(token) = lexer.tokenize(&mut pos) {
        assert_eq!(Some(token.ty()), expected_iter.next());
    }

    assert_eq!(pos, bytes.len());
    assert_eq!(None, expected_iter.next());
}

#[test]
fn tokenize_simple_1_xml() {
    tokenize(
        SIMPLE_1_XML_BYTES,
        &[
            Ty::ProcessingInstruction(ProcessingInstruction::from(
                r#"<?xml version="1.0"?>"#.as_bytes(),
            )),
            #[cfg(not(target_os = "windows"))]
            Ty::Characters(Characters::from("\n".as_bytes())),
            #[cfg(target_os = "windows")]
            Ty::Characters(Characters::from("\r\n".as_bytes())),
            Ty::StartTag(StartTag::from("<document>".as_bytes())),
            Ty::Characters(Characters::from("Hello world!".as_bytes())),
            Ty::EndTag(EndTag::from("</document>".as_bytes())),
        ],
    );
}

#[test]
fn tokenize_iter_simple_1_xml() {
    tokenize_via_iterator(
        SIMPLE_1_XML_BYTES,
        &[
            Ty::ProcessingInstruction(ProcessingInstruction::from(
                r#"<?xml version="1.0"?>"#.as_bytes(),
            )),
            #[cfg(not(target_os = "windows"))]
            Ty::Characters(Characters::from("\n".as_bytes())),
            #[cfg(target_os = "windows")]
            Ty::Characters(Characters::from("\r\n".as_bytes())),
            Ty::StartTag(StartTag::from("<document>".as_bytes())),
            Ty::Characters(Characters::from("Hello world!".as_bytes())),
            Ty::EndTag(EndTag::from("</document>".as_bytes())),
        ],
    );
}

#[test]
fn tokenize_svg_1_xml() {
    tokenize(
        SVG_1_XML_BYTES,
        #[cfg(not(target_os = "windows"))]
        &[
            Ty::ProcessingInstruction(ProcessingInstruction::from(
                r#"<?xml version="1.0"?>"#.as_bytes(),
            )),
            Ty::Characters(Characters::from("\n".as_bytes())),
            Ty::Declaration(Declaration::from("<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">".as_bytes())),
            Ty::Characters(Characters::from("\n\n".as_bytes())),
            Ty::StartTag(StartTag::from("<svg xmlns=\"http://www.w3.org/2000/svg\"\n     width=\"800\" height=\"800\">".as_bytes())),
            Ty::Characters(Characters::from("\n  ".as_bytes())),
            Ty::EmptyElementTag(EmptyElementTag::from("<circle cx=\"400\" cy=\"400\" r=\"50\" stroke=\"blue\"\n    stroke-width=\"1\" fill=\"yellow\" />".as_bytes())),
            Ty::Characters(Characters::from("\n".as_bytes())),
            Ty::EndTag(EndTag::from("</svg>".as_bytes())),
        ],
        #[cfg(target_os = "windows")]
        &[
            Ty::ProcessingInstruction(ProcessingInstruction::from(
                r#"<?xml version="1.0"?>"#.as_bytes(),
            )),
            Ty::Characters(Characters::from("\r\n".as_bytes())),
            Ty::Declaration(Declaration::from("<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\r\n  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">".as_bytes())),
            Ty::Characters(Characters::from("\r\n\r\n".as_bytes())),
            Ty::StartTag(StartTag::from("<svg xmlns=\"http://www.w3.org/2000/svg\"\r\n     width=\"800\" height=\"800\">".as_bytes())),
            Ty::Characters(Characters::from("\r\n  ".as_bytes())),
            Ty::EmptyElementTag(EmptyElementTag::from("<circle cx=\"400\" cy=\"400\" r=\"50\" stroke=\"blue\"\r\n    stroke-width=\"1\" fill=\"yellow\" />".as_bytes())),
            Ty::Characters(Characters::from("\r\n".as_bytes())),
            Ty::EndTag(EndTag::from("</svg>".as_bytes())),
        ],
    );
}

#[test]
fn tokenize_iter_svg_1_xml() {
    tokenize_via_iterator(
        SVG_1_XML_BYTES,
        #[cfg(not(target_os = "windows"))]
        &[
            Ty::ProcessingInstruction(ProcessingInstruction::from(
                r#"<?xml version="1.0"?>"#.as_bytes(),
            )),
            Ty::Characters(Characters::from("\n".as_bytes())),
            Ty::Declaration(Declaration::from("<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">".as_bytes())),
            Ty::Characters(Characters::from("\n\n".as_bytes())),
            Ty::StartTag(StartTag::from("<svg xmlns=\"http://www.w3.org/2000/svg\"\n     width=\"800\" height=\"800\">".as_bytes())),
            Ty::Characters(Characters::from("\n  ".as_bytes())),
            Ty::EmptyElementTag(EmptyElementTag::from("<circle cx=\"400\" cy=\"400\" r=\"50\" stroke=\"blue\"\n    stroke-width=\"1\" fill=\"yellow\" />".as_bytes())),
            Ty::Characters(Characters::from("\n".as_bytes())),
            Ty::EndTag(EndTag::from("</svg>".as_bytes())),
        ],
        #[cfg(target_os = "windows")]
        &[
            Ty::ProcessingInstruction(ProcessingInstruction::from(
                r#"<?xml version="1.0"?>"#.as_bytes(),
            )),
            Ty::Characters(Characters::from("\r\n".as_bytes())),
            Ty::Declaration(Declaration::from("<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\r\n  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">".as_bytes())),
            Ty::Characters(Characters::from("\r\n\r\n".as_bytes())),
            Ty::StartTag(StartTag::from("<svg xmlns=\"http://www.w3.org/2000/svg\"\r\n     width=\"800\" height=\"800\">".as_bytes())),
            Ty::Characters(Characters::from("\r\n  ".as_bytes())),
            Ty::EmptyElementTag(EmptyElementTag::from("<circle cx=\"400\" cy=\"400\" r=\"50\" stroke=\"blue\"\r\n    stroke-width=\"1\" fill=\"yellow\" />".as_bytes())),
            Ty::Characters(Characters::from("\r\n".as_bytes())),
            Ty::EndTag(EndTag::from("</svg>".as_bytes())),
        ],
    );
}
