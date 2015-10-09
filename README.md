`hashconsing` is a hash consing library in Rust.

It is a rather straightforward implementation of [a paper by Filiâtre and Conchon](paper).

For more details see [the documentation](doc).

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
hashconsing = "0.3.0"
```

and this to your crate root:

```rust
#[macro_use] // Optional, if you want to use the macros.
extern crate hashconsing;
```

[paper]: http://dl.acm.org/citation.cfm?doid=1159876.1159880 (Conchon et al.)
[doc]: http://adrienchampion.bitbucket.org/hashconsing/hashconsing/ (hashconsing documentation)