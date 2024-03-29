# MaybeXml

MaybeXml is a library to scan and evaluate [XML][xml]-like data into tokens. In
effect, the library provides a non-validating parser. The interface is similar to
many XML pull parsers.

* [Latest API Documentation][api_docs]

## Purpose

The purpose of the library is to provide a way to read XML documents including
office suite documents, RSS/Atom feeds, config files, SVG, and web service
messages.

## Examples

### Using `tokenize()`

```rust
use maybe_xml::{Reader, token::{Characters, EndTag, StartTag, Ty}};

let input = "<id>123</id>";

let reader = Reader::from_str(input);
let mut pos = 0;

let token = reader.tokenize(&mut pos);
if let Some(Ty::StartTag(tag)) = token.map(|t| t.ty()) {
    assert_eq!("id", tag.name().local().as_str());
    assert_eq!(None, tag.name().namespace_prefix());
} else {
    panic!();
}
assert_eq!(4, pos);

let token = reader.tokenize(&mut pos);
if let Some(Ty::Characters(chars)) = token.map(|t| t.ty()) {
    assert_eq!("123", chars.content().as_str());
} else {
    panic!();
}
assert_eq!(7, pos);

let token = reader.tokenize(&mut pos);
if let Some(Ty::EndTag(tag)) = token.map(|t| t.ty()) {
    assert_eq!("</id>", tag.as_str());
    assert_eq!("id", tag.name().local().as_str());
} else {
    panic!();
}
assert_eq!(12, pos);

let token = reader.tokenize(&mut pos);
assert_eq!(None, token);

// Verify that `pos` is equal to `input.len()` to ensure all data was
// processed.
```

### Using `Iterator` functionality

```rust
use maybe_xml::{Reader, token::Ty};

let input = "<id>123</id><name>Jane Doe</name>";

let reader = Reader::from_str(input);
let mut iter = reader.into_iter().filter_map(|token| {
    match token.ty() {
        Ty::StartTag(tag) => Some(tag.name().as_str()),
        _ => None,
    }
});

let name = iter.next();
assert_eq!(Some("id"), name);

let name = iter.next();
assert_eq!(Some("name"), name);

assert_eq!(None, iter.next());
```

## Installation

```sh
cargo add maybe_xml
```

By default, the `std` feature is enabled.

### Alloc only

If the host environment has an allocator but does not have access to the Rust
`std` library:

```sh
cargo add --no-default-features --features alloc maybe_xml
```

### No allocator / core only

If the host environment does not have an allocator:

```sh
cargo add --no-default-features maybe_xml
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