![crates.io](https://img.shields.io/crates/v/hashconsing.svg)
![Documentation](https://docs.rs/hashconsing/badge.svg)
![ci](https://github.com/AdrienChampion/hashconsing/workflows/ci/badge.svg)


# `hashconsing`

`hashconsing` is a Rust hash consing library.

It is based on [Type-Safe Modular Hash-Consing][paper] by Filliâtre and Conchon. It is slightly less
efficient as uses Rust's `HashMap`s, not a custom built structure.

For more details see [the documentation][doc].

# License

MIT/Apache-2.0

# Developers

- [@adrienchampion](https://github.com/adrienchampion), original author
- [@lenianiva](https://github.com/lenianiva), maintainer

## Contributors

- [@alex-ozdemir](https://github.com/alex-ozdemir)

[paper]: http://dl.acm.org/citation.cfm?doid=1159876.1159880 (Conchon et al.)
[doc]: https://docs.rs/hashconsing (hashconsing documentation)
[kino]: https://github.com/kino-mc/kino (kino on github)
[hoice]: https://github.com/hopv/hoice (hoice on github)