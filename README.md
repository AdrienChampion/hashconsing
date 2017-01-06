# `hashconsing`

[![Build Status](https://travis-ci.org/AdrienChampion/hashconsing.svg?branch=master)](https://travis-ci.org/AdrienChampion/hashconsing) [![Latest Version](https://img.shields.io/crates/v/rsmt2.svg)](https://crates.io/crates/rsmt2)

`hashconsing` is a hash consing library in Rust.

It is a based on [Type-Safe Modular Hash-Consing](paper) by Filiâtre and
Conchon. It is slightly less efficient as uses Rust's `HashMap`s, not a custom
built structure.

For more details see [the documentation](doc).

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
hashconsing = "0.5.0"
```

and this to your crate root:

```rust
extern crate hashconsing ;
```

[paper]: http://dl.acm.org/citation.cfm?doid=1159876.1159880 (Conchon et al.)
[doc]: https://docs.rs/hashconsing (hashconsing documentation)