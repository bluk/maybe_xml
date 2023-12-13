use std::io;

use maybe_xml::{token, Reader};

#[derive(Debug, Default)]
struct TokenCounters {
    start_tag: usize,
    empty_element_tag: usize,
    end_tag: usize,
    chars: usize,
    pi: usize,
    decl: usize,
    comment: usize,
    cdata: usize,
}

fn main() -> io::Result<()> {
    let stdin = io::read_to_string(io::stdin())?;

    let reader = Reader::from_str(&stdin);

    let mut counters = TokenCounters::default();

    for token in reader {
        match token.ty() {
            token::Ty::StartTag(_) => {
                counters.start_tag += 1;
            }
            token::Ty::EmptyElementTag(_) => {
                counters.empty_element_tag += 1;
            }
            token::Ty::EndTag(_) => {
                counters.end_tag += 1;
            }
            token::Ty::Characters(_) => {
                counters.chars += 1;
            }
            token::Ty::ProcessingInstruction(_) => {
                counters.pi += 1;
            }
            token::Ty::Declaration(_) => {
                counters.decl += 1;
            }
            token::Ty::Comment(_) => {
                counters.comment += 1;
            }
            token::Ty::Cdata(_) => {
                counters.cdata += 1;
            }
        }
    }

    println!("Start Tag: {}", counters.start_tag);
    println!("Empty Element Tag: {}", counters.empty_element_tag);
    println!("End Tag: {}", counters.end_tag);
    println!("Characters: {}", counters.chars);
    println!("Processing Instruction: {}", counters.pi);
    println!("Declaration: {}", counters.decl);
    println!("Comment: {}", counters.comment);
    println!("Cdata: {}", counters.cdata);

    Ok(())
}
