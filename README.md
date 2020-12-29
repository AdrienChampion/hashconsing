![CI status](https://github.com/actions/hashconsing/workflows/.github/workflows/rust.yml/badge.svg)

# `hashconsing`

`hashconsing` is a hash consing library in Rust.

It is based on [Type-Safe Modular Hash-Consing][paper] by Filliâtre and Conchon. It is slightly less
efficient as uses Rust's `HashMap`s, not a custom built structure.

For more details see [the documentation][doc].

# Known projects using `hashconsing`

- [kinō][kino], a model-checker for transition systems
- [hoice][hoice], a machine-learning-based predicate synthesizer for horn clauses

# License

MIT/Apache-2.0

[paper]: http://dl.acm.org/citation.cfm?doid=1159876.1159880 (Conchon et al.)
[doc]: https://docs.rs/hashconsing (hashconsing documentation)
[kino]: https://github.com/kino-mc/kino (kino on github)
[hoice]: https://github.com/hopv/hoice (hoice on github)