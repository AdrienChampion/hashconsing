# `hashconsing`

[![Build Status](https://travis-ci.org/AdrienChampion/hashconsing.svg?branch=master)](https://travis-ci.org/AdrienChampion/hashconsing) [![Latest Version](https://img.shields.io/crates/v/hashconsing.svg)](https://crates.io/crates/hashconsing)

`hashconsing` is a hash consing library in Rust.

It is a based on [Type-Safe Modular Hash-Consing](paper) by Filiâtre and
Conchon. It is slightly less efficient as uses Rust's `HashMap`s, not a custom
built structure.

For more details see [the documentation](doc).

# License

MIT/Apache-2.0

[paper]: http://dl.acm.org/citation.cfm?doid=1159876.1159880 (Conchon et al.)
[doc]: https://docs.rs/hashconsing (hashconsing documentation)