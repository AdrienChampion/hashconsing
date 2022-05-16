//! All usable hashers.
//!
//! More details available in [PR 8].
//!
//! [PR 8]: https://github.com/AdrienChampion/hashconsing/pull/8 (PR 8 on github)

/// Hasher and hasher factory from [`ahash`].
///
/// > **NB:** this module is empty if the `with_ahash` feature is not active.
///
/// [`ahash`]: https://crates.io/crates/ahash (ahash on crates.io)
#[cfg(feature = "with_ahash")]
pub mod a_hash {
    pub use ahash::{AHasher as Hasher, RandomState as Builder};
}

/// Hasher and hasher factory for `std`'s `sip-hash.
pub mod sip_hash {
    pub use std::collections::hash_map::{DefaultHasher as Hasher, RandomState as Builder};
}

/// Hasher and hasher factory for `p-hash`.
pub mod p_hash {
    use std::hash::{BuildHasher as StdBuilderExt, Hasher as StdHasherExt};

    /// P-hash-er factory.
    #[derive(Clone, Debug)]
    pub struct Builder {}
    impl Builder {
        /// Empty factory constructor.
        pub fn new() -> Self {
            Self {}
        }
    }
    impl StdBuilderExt for Builder {
        type Hasher = Hasher;
        fn build_hasher(&self) -> Hasher {
            Hasher { buf: [0; 8] }
        }
    }
    impl Default for Builder {
        fn default() -> Self {
            Builder {}
        }
    }

    /// Simple hasher for `usize`.
    ///
    /// Multiplies the number by: `0xDA5DF7A7BD02F2C7u64`, a 64-bit prime.
    ///
    /// **This hasher is only for hashing `usize`-s**. Any other use is **unsafe**.
    pub struct Hasher {
        buf: [u8; 8],
    }
    impl Hasher {
        /// Checks that a slice of bytes has the length of a `usize`. Only active
        /// in debug.
        #[cfg(debug)]
        #[inline(always)]
        fn test_bytes(bytes: &[u8]) {
            if bytes.len() != 8 {
                panic!(
                    "[illegal] p-hash-er \
                     called with non-`u64` argument ({} bytes, expected {})",
                    bytes.len(),
                    8
                )
            }
        }
        /// Checks that a slice of bytes has the length of a `usize`. Only active
        /// in debug.
        #[cfg(not(debug))]
        #[inline(always)]
        fn test_bytes(_: &[u8]) {}
    }
    impl StdHasherExt for Hasher {
        fn finish(&self) -> u64 {
            let block: u64 = unsafe { ::std::mem::transmute(self.buf) };
            // Multiply by random 64-bit prime to distribute
            block.wrapping_mul(0xDA5DF7A7BD02F2C7u64)
        }
        fn write(&mut self, bytes: &[u8]) {
            Self::test_bytes(bytes);
            self.buf[..8].clone_from_slice(&bytes[..8])
        }
    }
}

/// Hasher and hasher factory for the identity hasher.
pub mod id_hash {
    use std::hash::{BuildHasher as StdBuilderExt, Hasher as StdHasherExt};

    /// P-hash-er factory.
    #[derive(Clone, Debug)]
    pub struct Builder {}
    impl Builder {
        /// Empty factory constructor.
        pub fn new() -> Self {
            Self {}
        }
    }
    impl StdBuilderExt for Builder {
        type Hasher = Hasher;
        fn build_hasher(&self) -> Hasher {
            Hasher { buf: [0; 8] }
        }
    }
    impl Default for Builder {
        fn default() -> Self {
            Builder {}
        }
    }

    /// Simple hasher for `usize`.
    ///
    /// Multiplies the number by: `0xDA5DF7A7BD02F2C7u64`, a 64-bit prime.
    ///
    /// **This hasher is only for hashing `usize`-s**. Any other use is **unsafe**.
    pub struct Hasher {
        buf: [u8; 8],
    }
    impl Hasher {
        /// Checks that a slice of bytes has the length of a `usize`. Only active
        /// in debug.
        #[cfg(debug)]
        #[inline(always)]
        fn test_bytes(bytes: &[u8]) {
            if bytes.len() != 8 {
                panic!(
                    "[illegal] p-hash-er \
                     called with non-`u64` argument ({} bytes, expected {})",
                    bytes.len(),
                    8
                )
            }
        }
        /// Checks that a slice of bytes has the length of a `usize`. Only active
        /// in debug.
        #[cfg(not(debug))]
        #[inline(always)]
        fn test_bytes(_: &[u8]) {}
    }
    impl StdHasherExt for Hasher {
        fn finish(&self) -> u64 {
            unsafe { ::std::mem::transmute(self.buf) }
        }
        fn write(&mut self, bytes: &[u8]) {
            Self::test_bytes(bytes);
            self.buf[..8].clone_from_slice(&bytes[..8])
        }
    }
}
