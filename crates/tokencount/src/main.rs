use std::io;

use clap::{Parser, Subcommand};
use maybe_xml::ScanDocumentOpts;

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

#[derive(Debug, Parser)]
struct Args {
    #[command(subcommand)]
    cmd: Option<Cmd>,
}

#[derive(Debug, Subcommand)]
enum Cmd {
    Count,
    VerifyStrictXml,
    VerifyRelaxed,
    VerifyAssumeXml,
}

#[inline]
#[must_use]
fn is_utf8_bom(input: &[u8]) -> bool {
    if input.len() < 4 {
        return false;
    }

    input[0] == 0xEF && input[1] == 0xBB && input[2] == 0xBF
}

#[inline]
#[must_use]
fn complete_scan(input: &[u8], opts: ScanDocumentOpts) -> bool {
    let input = if is_utf8_bom(input) {
        &input[3..]
    } else {
        input
    };

    maybe_xml::scan_document(input, 0, opts) == Some(input.len())
}

fn main() -> io::Result<()> {
    let args = Args::parse();

    let stdin = io::read_to_string(io::stdin())?;

    match args.cmd {
        Some(Cmd::Count) | None => {
            let mut counters = TokenCounters::default();

            let reader = Reader::from_str(&stdin);

            let mut pos = 0;

            while let Some(token) = reader.tokenize(&mut pos) {
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

            if pos != stdin.len() {
                let error =
                    format!("should have read the entire stdin but only read to {pos} bytes");
                return Err(io::Error::new(io::ErrorKind::Other, error));
            }

            println!("Start Tag: {}", counters.start_tag);
            println!("Empty Element Tag: {}", counters.empty_element_tag);
            println!("End Tag: {}", counters.end_tag);
            println!("Characters: {}", counters.chars);
            println!("Processing Instruction: {}", counters.pi);
            println!("Declaration: {}", counters.decl);
            println!("Comment: {}", counters.comment);
            println!("Cdata: {}", counters.cdata);
        }
        Some(Cmd::VerifyStrictXml) => {
            if !complete_scan(stdin.as_bytes(), ScanDocumentOpts::new()) {
                let error = "scan_document with DEFAULT options should have read the entire stdin";
                return Err(io::Error::new(io::ErrorKind::Other, error));
            }
        }
        Some(Cmd::VerifyRelaxed) => {
            if !complete_scan(stdin.as_bytes(), ScanDocumentOpts::relaxed()) {
                let error = "scan_document with RELAXED options should have read the entire stdin";
                return Err(io::Error::new(io::ErrorKind::Other, error));
            }
        }
        Some(Cmd::VerifyAssumeXml) => {
            if !complete_scan(stdin.as_bytes(), ScanDocumentOpts::assume_valid_xml()) {
                let error =
                    "scan_document with ASSUME VALID XML options should have read the entire stdin";
                return Err(io::Error::new(io::ErrorKind::Other, error));
            }
        }
    }

    println!("OK");

    Ok(())
}
