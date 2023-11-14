use maybe_xml::{token::Ty, Reader};

const SIMPLE_1_XML: &str = include_str!("../tests/resources/simple-1.xml");
const SVG_1_XML: &str = include_str!("../tests/resources/svg-1.xml");

macro_rules! test_iter {
    ($iter:expr ;)  => {};
    ($iter:expr ; [$expected_txt:literal, $expected_ty:pat] $(,)?) => {
        let token = $iter.next();
        assert_eq!(token.map(|t| t.as_str()), Some($expected_txt));
        assert!(matches!(token.map(|t| t.ty()), Some($expected_ty)));
    };
    ($iter:expr; [$expected_txt:literal, $expected_ty:pat], $([$exp_txt_y:literal, $exp_ty_y:pat]),+ $(,)?) => {
        test_iter!($iter; [$expected_txt, $expected_ty]);
        test_iter!($iter; $([$exp_txt_y, $exp_ty_y]),+);
    };
    ($xml_str:expr, $([$exp_txt_y:literal, $exp_ty_y:pat]),+ $(,)?) => {
        let reader = Reader::from_str($xml_str);
        let mut iter = reader.iter(0);

        test_iter!(iter; $([$exp_txt_y, $exp_ty_y]),+);

        assert_eq!(None, iter.next());
    }
}

macro_rules! test_tokenize {
    ($reader:expr, $pos:expr ;)  => {};
    ($reader:expr, $pos:expr ; [$expected_txt:literal, $expected_ty:pat] $(,)?) => {
        let token = $reader.tokenize(&mut $pos);
        assert_eq!(token.map(|t| t.as_str()), Some($expected_txt));
        assert!(matches!(token.map(|t| t.ty()), Some($expected_ty)));
    };
    ($reader:expr, $pos:expr ; [$expected_txt:literal, $expected_ty:pat], $([$exp_txt_y:literal, $exp_ty_y:pat]),+ $(,)?) => {
        test_tokenize!($reader, $pos; [$expected_txt, $expected_ty]);
        test_tokenize!($reader, $pos; $([$exp_txt_y, $exp_ty_y]),+);
    };
    ($xml_str:expr, $([$exp_txt_y:literal, $exp_ty_y:pat]),+ $(,)?) => {
        let reader = Reader::from_str($xml_str);
        let mut pos = 0;

        test_tokenize!(reader, pos ; $([$exp_txt_y, $exp_ty_y]),+);

        assert_eq!(pos, $xml_str.len());
    }
}

#[test]
fn tokenize_simple_1_xml() {
    #[cfg(not(target_os = "windows"))]
    test_tokenize!(
        SIMPLE_1_XML,
        [r#"<?xml version="1.0"?>"#, Ty::ProcessingInstruction(_)],
        ["\n", Ty::Characters(_)],
        ["<document>", Ty::StartTag(_)],
        ["Hello world!", Ty::Characters(_)],
        ["</document>", Ty::EndTag(_)],
    );

    #[cfg(target_os = "windows")]
    test_tokenize!(
        SIMPLE_1_XML,
        [r#"<?xml version="1.0"?>"#, Ty::ProcessingInstruction(_)],
        ["\r\n", Ty::Characters(_)],
        ["<document>", Ty::StartTag(_)],
        ["Hello world!", Ty::Characters(_)],
        ["</document>", Ty::EndTag(_)],
    );
}

#[test]
fn tokenize_iter_simple_1_xml() {
    #[cfg(not(target_os = "windows"))]
    test_iter!(
        SIMPLE_1_XML,
        [r#"<?xml version="1.0"?>"#, Ty::ProcessingInstruction(_)],
        ["\n", Ty::Characters(_)],
        ["<document>", Ty::StartTag(_)],
        ["Hello world!", Ty::Characters(_)],
        ["</document>", Ty::EndTag(_)],
    );

    #[cfg(target_os = "windows")]
    test_iter!(
        SIMPLE_1_XML,
        [r#"<?xml version="1.0"?>"#, Ty::ProcessingInstruction(_)],
        ["\r\n", Ty::Characters(_)],
        ["<document>", Ty::StartTag(_)],
        ["Hello world!", Ty::Characters(_)],
        ["</document>", Ty::EndTag(_)],
    );
}

#[test]
fn tokenize_svg_1_xml() {
    #[cfg(not(target_os = "windows"))]
    test_tokenize!(
        SVG_1_XML,
        [r#"<?xml version="1.0"?>"#, Ty::ProcessingInstruction(_)],
        ["\n", Ty::Characters(_)],
        ["<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">", Ty::Declaration(_)],
        ["\n\n", Ty::Characters(_)],
        ["<svg xmlns=\"http://www.w3.org/2000/svg\"\n     width=\"800\" height=\"800\">", Ty::StartTag(_)],
        ["\n  ", Ty::Characters(_)],
        ["<circle cx=\"400\" cy=\"400\" r=\"50\" stroke=\"blue\"\n    stroke-width=\"1\" fill=\"yellow\" />", Ty::EmptyElementTag(_)],
        ["\n", Ty::Characters(_)],
        ["</svg>", Ty::EndTag(_)],
    );

    #[cfg(target_os = "windows")]
    test_tokenize!(
        SVG_1_XML,
        [r#"<?xml version="1.0"?>"#, Ty::ProcessingInstruction(_)],
        ["\r\n", Ty::Characters(_)],
        ["<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\r\n  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">", Ty::Declaration(_)],
        ["\r\n\r\n", Ty::Characters(_)],
        ["<svg xmlns=\"http://www.w3.org/2000/svg\"\r\n     width=\"800\" height=\"800\">", Ty::StartTag(_)],
        ["\r\n  ", Ty::Characters(_)],
        ["<circle cx=\"400\" cy=\"400\" r=\"50\" stroke=\"blue\"\r\n    stroke-width=\"1\" fill=\"yellow\" />", Ty::EmptyElementTag(_)],
        ["\r\n", Ty::Characters(_)],
        ["</svg>", Ty::EndTag(_)],
    );
}

#[test]
fn tokenize_iter_svg_1_xml() {
    #[cfg(not(target_os = "windows"))]
    test_iter!(
        SVG_1_XML,
        [r#"<?xml version="1.0"?>"#, Ty::ProcessingInstruction(_)],
        ["\n", Ty::Characters(_)],
        ["<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">", Ty::Declaration(_)],
        ["\n\n", Ty::Characters(_)],
        ["<svg xmlns=\"http://www.w3.org/2000/svg\"\n     width=\"800\" height=\"800\">", Ty::StartTag(_)],
        ["\n  ", Ty::Characters(_)],
        ["<circle cx=\"400\" cy=\"400\" r=\"50\" stroke=\"blue\"\n    stroke-width=\"1\" fill=\"yellow\" />", Ty::EmptyElementTag(_)],
        ["\n", Ty::Characters(_)],
        ["</svg>", Ty::EndTag(_)],
    );

    #[cfg(target_os = "windows")]
    test_iter!(
        SVG_1_XML,
        [r#"<?xml version="1.0"?>"#, Ty::ProcessingInstruction(_)],
        ["\r\n", Ty::Characters(_)],
        ["<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\r\n  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">", Ty::Declaration(_)],
        ["\r\n\r\n", Ty::Characters(_)],
        ["<svg xmlns=\"http://www.w3.org/2000/svg\"\r\n     width=\"800\" height=\"800\">", Ty::StartTag(_)],
        ["\r\n  ", Ty::Characters(_)],
        ["<circle cx=\"400\" cy=\"400\" r=\"50\" stroke=\"blue\"\r\n    stroke-width=\"1\" fill=\"yellow\" />", Ty::EmptyElementTag(_)],
        ["\r\n", Ty::Characters(_)],
        ["</svg>", Ty::EndTag(_)],
    );
}
