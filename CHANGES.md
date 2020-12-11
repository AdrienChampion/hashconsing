v1.2.0

- cleaned `lazy_static` re-export, now only re-exporting `lazy_static` macro
- added a `capacity` method on the consign
- added `collect`, `shrink_to_fit`, `collect_to_fit` and `reserve` to the `HashConsign` trait

v1.1.0

- removed systematic unsafe implementations of `Send` and `Sync` for `HConsed`,
    see [issue 1](https://github.com/AdrienChampion/hashconsing/issues/1)

v1.0.1

- fixed path bug with `lazy_static` in `consign` macro

v1.0.0

- removed WIP `new_hconsed` macro (I found no way to make it generic enough)
- renamed the factory creation macro `new_consign` to just `consign`

v0.9.11

- more homogeneous struct/trait naming
- documentation effort
- lazy static consign creation macro