use criterion::{criterion_group, criterion_main, Criterion};

const SIMPLE_1: &str = include_str!("../../maybe_xml/tests/resources/simple-1.xml");
const SVG_1: &str = include_str!("../../maybe_xml/tests/resources/svg-1.xml");
const RSS_1: &str = include_str!("../../maybe_xml/tests/resources/rss-1.xml");
const LARGE_1: &str = include_str!("../../maybe_xml/tests/resources/large-1.xml");

fn lexer_into_iter(input: &str) -> u64 {
    use maybe_xml::{token::Ty, Lexer};

    let lexer = Lexer::from_str(input);
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
    c.bench_function("lexer_into_iter_simple_xml_1", |b| {
        b.iter(|| {
            let count = lexer_into_iter(SIMPLE_1);
            assert_eq!(1, count);
        });
    });
    c.bench_function("lexer_into_iter_svg_1", |b| {
        b.iter(|| {
            let count = lexer_into_iter(SVG_1);
            assert_eq!(1, count);
        });
    });
    c.bench_function("lexer_into_iter_rss_1", |b| {
        b.iter(|| {
            let count = lexer_into_iter(RSS_1);
            assert_eq!(36, count);
        });
    });
    c.bench_function("lexer_into_iter_large_1", |b| {
        b.iter(|| {
            let count = lexer_into_iter(LARGE_1);
            assert_eq!(count, 9885);
        });
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
