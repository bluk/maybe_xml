# MaybeXml

MaybeXml is a library to scan and evaluate [XML][xml]-like data into tokens. In
effect, the library provides a non-validating lexer. The interface is similar to many
XML pull parsers.

* [Latest API Documentation][api_docs]

## Usage

The library user creates a `Lexer` from a slice of bytes. The slice of
bytes is usually from a buffer managed by the library user. For instance, it
could be a buffer of data that is being read over a network socket, a memory
mapped file, or just a `Vec` of bytes.

Then, the library user can call `Lexer::tokenize()` to try to get the next
`Token`. If successful, repeat calling `tokenize` and process the available
tokens.

Alternatively, the user can turn the `Lexer` into an iterator via
`Lexer::iter()` or `IntoIterator::into_iter()`.

## Purpose

The purpose of the library is to provide a way to read XML documents including
office suite documents, RSS/Atom feeds, config files, SVG, and web service messages.

## Installation

By default, the `std` feature is enabled.

```toml
[dependencies]
maybe_xml = "0.5.0"
```

Currently, all functionality is available even with `no alloc` and `no std`. The
library does not allocate or use features available only in the `alloc` or `std` libraries.
Future functionality may require `alloc` or `std`, so please set the appropriate feature set.

### Alloc only

If the host environment has an allocator but does not have access to the Rust `std` library:

```toml
[dependencies]
maybe_xml = { version = "0.5.0", default-features = false, features = ["alloc"]}
```

### No allocator / core only

If the host environment does not have an allocator:

```toml
[dependencies]
maybe_xml = { version = "0.5.0", default-features = false }
```

## Examples

### Using `Iterator` functionality

```rust
use maybe_xml::{Lexer, token::{Characters, EndTag, StartTag, Token, Ty}};

let input = b"<id>Example</id>";

let lexer = Lexer::from_slice(input);

let mut iter = lexer.into_iter().map(|token| token.ty());

let token_type = iter.next();
assert_eq!(token_type, Some(Ty::StartTag(StartTag::from("<id>"))));
match token_type {
    Some(Ty::StartTag(start_tag)) => {
        assert_eq!(start_tag.name().to_str()?, "id");
    }
    _ => panic!("unexpected token"),
}
assert_eq!(iter.next(), Some(Ty::Characters(Characters::from("Example"))));
assert_eq!(iter.next(), Some(Ty::EndTag(EndTag::from("</id>"))));
assert_eq!(iter.next(), None);
# Ok::<(), core::str::Utf8Error>(())
```

### Using `Lexer::tokenize()` directly

```rust
use maybe_xml::{Lexer, token::{Characters, EndTag, StartTag, Ty}};

let mut buf = Vec::new();
// Note the missing closing tag character `>` in the end tag.
buf.extend(b"<id>123</id");

let lexer = Lexer::from_slice(&buf);
let mut pos = 0;

let token = lexer.tokenize(&mut pos).unwrap();
assert_eq!(0, token.offset());
assert_eq!(Ty::StartTag(StartTag::from("<id>".as_bytes())), token.ty());

let token = lexer.tokenize(&mut pos).unwrap();
assert_eq!(4, token.offset());
assert_eq!(Ty::Characters(Characters::from("123".as_bytes())), token.ty());

let token = lexer.tokenize(&mut pos);
// The last token is incomplete because it is missing the `>`
assert_eq!(None, token);

// The position was updated as tokenize() was called
assert_eq!(7, pos);

// Remove the tokenized data and reset the position
buf.drain(..pos);
pos = 0;

// Verify that the buffer is empty. If it is not empty, then there is data
// which could not be identified as a complete token. This usually indicates
// an error has occurred. If there is more data (say coming from a network
// socket), then append the new data when it becomes available and call
// tokenize again.
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