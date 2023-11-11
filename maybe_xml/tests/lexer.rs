use maybe_xml::{
    token::{
        Characters, Declaration, EmptyElementTag, EndTag, ProcessingInstruction, StartTag, Ty,
    },
    Lexer,
};

const SIMPLE_1_XML: &str = include_str!("../tests/resources/simple-1.xml");
const SVG_1_XML: &str = include_str!("../tests/resources/svg-1.xml");

fn tokenize_via_iterator(input: &str, expected_tokens: &[Ty<'_>]) {
    let lexer = Lexer::from_str(input);

    for (expected_token, token) in expected_tokens.iter().zip(lexer.iter(0)) {
        assert_eq!(*expected_token, token.ty());
    }

    assert_eq!(lexer.into_iter().count(), expected_tokens.len());
}

fn tokenize(input: &str, expected_tokens: &[Ty<'_>]) {
    let lexer = Lexer::from_str(input);
    let mut pos = 0;

    let mut expected_iter = expected_tokens.iter().copied();

    while let Some(token) = lexer.tokenize(&mut pos) {
        assert_eq!(Some(token.ty()), expected_iter.next());
    }

    assert_eq!(pos, input.len());
    assert_eq!(None, expected_iter.next());
}

#[test]
fn tokenize_simple_1_xml() {
    tokenize(
        SIMPLE_1_XML,
        &[
            Ty::ProcessingInstruction(ProcessingInstruction::from_str(r#"<?xml version="1.0"?>"#)),
            #[cfg(not(target_os = "windows"))]
            Ty::Characters(Characters::from_str("\n")),
            #[cfg(target_os = "windows")]
            Ty::Characters(Characters::from("\r\n")),
            Ty::StartTag(StartTag::from_str("<document>")),
            Ty::Characters(Characters::from_str("Hello world!")),
            Ty::EndTag(EndTag::from_str("</document>")),
        ],
    );
}

#[test]
fn tokenize_iter_simple_1_xml() {
    tokenize_via_iterator(
        SIMPLE_1_XML,
        &[
            Ty::ProcessingInstruction(ProcessingInstruction::from_str(r#"<?xml version="1.0"?>"#)),
            #[cfg(not(target_os = "windows"))]
            Ty::Characters(Characters::from_str("\n")),
            #[cfg(target_os = "windows")]
            Ty::Characters(Characters::from("\r\n")),
            Ty::StartTag(StartTag::from_str("<document>")),
            Ty::Characters(Characters::from_str("Hello world!")),
            Ty::EndTag(EndTag::from_str("</document>")),
        ],
    );
}

#[test]
fn tokenize_svg_1_xml() {
    tokenize(
        SVG_1_XML,
        #[cfg(not(target_os = "windows"))]
        &[
            Ty::ProcessingInstruction(ProcessingInstruction::from_str(
                r#"<?xml version="1.0"?>"#,
            )),
            Ty::Characters(Characters::from_str("\n")),
            Ty::Declaration(Declaration::from_str("<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">")),
            Ty::Characters(Characters::from_str("\n\n")),
            Ty::StartTag(StartTag::from_str("<svg xmlns=\"http://www.w3.org/2000/svg\"\n     width=\"800\" height=\"800\">")),
            Ty::Characters(Characters::from_str("\n  ")),
            Ty::EmptyElementTag(EmptyElementTag::from_str("<circle cx=\"400\" cy=\"400\" r=\"50\" stroke=\"blue\"\n    stroke-width=\"1\" fill=\"yellow\" />")),
            Ty::Characters(Characters::from_str("\n")),
            Ty::EndTag(EndTag::from_str("</svg>")),
        ],
        #[cfg(target_os = "windows")]
        &[
            Ty::ProcessingInstruction(ProcessingInstruction::from(
                r#"<?xml version="1.0"?>"#,
            )),
            Ty::Characters(Characters::from_str("\r\n")),
            Ty::Declaration(Declaration::from_str("<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\r\n  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">")),
            Ty::Characters(Characters::from_str("\r\n\r\n")),
            Ty::StartTag(StartTag::from_str("<svg xmlns=\"http://www.w3.org/2000/svg\"\r\n     width=\"800\" height=\"800\">")),
            Ty::Characters(Characters::from_str("\r\n  ")),
            Ty::EmptyElementTag(EmptyElementTag::from_str("<circle cx=\"400\" cy=\"400\" r=\"50\" stroke=\"blue\"\r\n    stroke-width=\"1\" fill=\"yellow\" />")),
            Ty::Characters(Characters::from_str("\r\n")),
            Ty::EndTag(EndTag::from_str("</svg>")),
        ],
    );
}

#[test]
fn tokenize_iter_svg_1_xml() {
    tokenize_via_iterator(
        SVG_1_XML,
        #[cfg(not(target_os = "windows"))]
        &[
            Ty::ProcessingInstruction(ProcessingInstruction::from_str(
                r#"<?xml version="1.0"?>"#,
            )),
            Ty::Characters(Characters::from_str("\n")),
            Ty::Declaration(Declaration::from_str("<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">")),
            Ty::Characters(Characters::from_str("\n\n")),
            Ty::StartTag(StartTag::from_str("<svg xmlns=\"http://www.w3.org/2000/svg\"\n     width=\"800\" height=\"800\">")),
            Ty::Characters(Characters::from_str("\n  ")),
            Ty::EmptyElementTag(EmptyElementTag::from_str("<circle cx=\"400\" cy=\"400\" r=\"50\" stroke=\"blue\"\n    stroke-width=\"1\" fill=\"yellow\" />")),
            Ty::Characters(Characters::from_str("\n")),
            Ty::EndTag(EndTag::from_str("</svg>")),
        ],
        #[cfg(target_os = "windows")]
        &[
            Ty::ProcessingInstruction(ProcessingInstruction::from_str(
                r#"<?xml version="1.0"?>"#,
            )),
            Ty::Characters(Characters::from_str("\r\n")),
            Ty::Declaration(Declaration::from_str("<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\r\n  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">")),
            Ty::Characters(Characters::from_str("\r\n\r\n")),
            Ty::StartTag(StartTag::from_str("<svg xmlns=\"http://www.w3.org/2000/svg\"\r\n     width=\"800\" height=\"800\">")),
            Ty::Characters(Characters::from_str("\r\n  ")),
            Ty::EmptyElementTag(EmptyElementTag::from_str("<circle cx=\"400\" cy=\"400\" r=\"50\" stroke=\"blue\"\r\n    stroke-width=\"1\" fill=\"yellow\" />")),
            Ty::Characters(Characters::from_str("\r\n")),
            Ty::EndTag(EndTag::from_str("</svg>")),
        ],
    );
}
