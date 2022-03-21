# MaybeXml

MaybeXml is a library to scan and evaluate [XML][xml]-like data into tokens. In
effect, the library provides a non-validating lexer. The interface is similar to many
XML pull parsers.

* [Latest API Documentation][api_docs]

The library does 3 things:

1. A `Scanner` receives byte slices and identifies the start and end of tokens like
   tags, character content, and declarations.

2. An `Evaluator` transforms bytes from an input source (like instances of types which implement
   `std::io::BufRead`) into complete tokens via either a cursor or an iterator pull
   style API.

   From an implementation point of view, when a library user asks an
   `Evaluator` for the next token, the `Evaluator` reads the input and passes the
   bytes to an internal `Scanner`. The `Evaluator` buffers the scanned bytes and keeps reading
   until the `Scanner` determines a token has been completely read. Then all of the bytes
   which represent the token are returned to the library user as a variant of a token type.

3. Each token type provides methods which can provide views into the underlying bytes.
   For instance, a tag token could provide a `name()` method which returns a `TagName`.
   The `TagName` provides a method like `to_str()` which can be called to get a `str`
   representation of the tag name.

## Purpose

The purpose of the library is to provide a way to read XML documents including
office suite documents, RSS/Atom feeds, config files, SVG, and web service messages.

## Installation

By default, features which depend on the Rust `std` library are included.

```toml
[dependencies]
maybe_xml = "0.2.0"
```

### Alloc Only

If the host environment has an allocator but does not have access to the Rust `std` library:

```toml
[dependencies]
maybe_xml = { version = "0.2.0", default-features = false, features = ["alloc"]}
```

Most of the library, except for `Evaluator`s which rely on `std` types (such as `std::io::BufRead`),
is still available.

### No allocator

If the host environment does not have an allocator:

```toml
[dependencies]
maybe_xml = { version = "0.2.0", default-features = false }
```

The `Scanner` and the borrowed versions of the tokens are available.

## Example

The following is a short example showing the iterator API. The full example with all the module imports and error handling is in the `lib.rs` source file.

```rust
use maybe_xml::token::owned::{Token, StartTag, Characters, EndTag};

let mut input = std::io::BufReader::new(r#"<ID>Example</ID>"#.as_bytes());

let eval = maybe_xml::eval::bufread::BufReadEvaluator::from_reader(input);

let mut iter = eval.into_iter()
    .map(|token| match token {
        Token::StartTag(start_tag) => {
            if let Ok(str) = start_tag.to_str() {
                Token::StartTag(StartTag::from(str.to_lowercase()))
            } else {
                Token::StartTag(start_tag)
            }
        }
        Token::EndTag(end_tag) => {
            if let Ok(str) = end_tag.to_str() {
                Token::EndTag(EndTag::from(str.to_lowercase()))
            } else {
                Token::EndTag(end_tag)
            }
        }
        _ => token,
    });

let token = iter.next();
assert_eq!(token, Some(Token::StartTag(StartTag::from("<id>"))));
match token {
    Some(Token::StartTag(start_tag)) => {
        assert_eq!(start_tag.name().to_str()?, "id");
    }
    _ => panic!("unexpected token"),
}
assert_eq!(iter.next(), Some(Token::Characters(Characters::from("Example"))));
assert_eq!(iter.next(), Some(Token::EndTag(EndTag::from("</id>"))));
assert_eq!(iter.next(), Some(Token::Eof));
assert_eq!(iter.next(), None);
```

## License

Licensed under either of [Apache License, Version 2.0][LICENSE_APACHE] or [MIT
License][LICENSE_MIT] at your option.

### Contributions

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.

[LICENSE_APACHE]: LICENSE-APACHE
[LICENSE_MIT]: LICENSE-MIT
[xml]: https://www.w3.org/TR/2006/REC-xml11-20060816/
[api_docs]: https://docs.rs/maybe_xml/