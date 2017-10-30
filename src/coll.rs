//! Efficient collections for hashconsed data.
//!
//! This module provide hash set and hash map types with trivial hash functions
//! for hashconsed types. The hash of an hashconsed value is its unique
//! identifier, verbatim. This is obviously extremely dangerous from a security
//! point of view: these collections should **never** be used for cryptographic
//! purposes.

use std::collections::{ HashSet, HashMap } ;
use std::ops::{ Deref, DerefMut } ;
use std::hash::Hash ;

use { HConsed, HashConsed } ;
use self::hash::BuildHashU64 ;

/// A hash set of hash-consed things with trivial hashing.
pub struct HConSet<T: HashConsed> {
  set: HashSet< HConsed<T::Inner>, BuildHashU64 >
}
impl<T: HashConsed> HConSet<T>
where T::Inner: Hash {
  /// An empty set of hashconsed things.
  #[inline]
  pub fn new() -> Self {
    HConSet {
      set: HashSet::with_hasher(BuildHashU64 {})
    }
  }
  /// An empty set of hashconsed things with a capacity.
  #[inline]
  pub fn with_capacity(capa: usize) -> Self {
    HConSet {
      set: HashSet::with_capacity_and_hasher(capa, BuildHashU64 {})
    }
  }
}
impl<T: HashConsed> Deref for HConSet<T> {
  type Target = HashSet<HConsed<T::Inner>, BuildHashU64> ;
  fn deref(& self) -> & Self::Target {
    & self.set
  }
}
impl<T: HashConsed> DerefMut for HConSet<T> {
  fn deref_mut(& mut self) -> & mut Self::Target {
    & mut self.set
  }
}

/// A hash map of hash-consed things with trivial hashing.
pub struct HConMap<T: HashConsed, V> {
  map: HashMap< HConsed<T::Inner>, V, BuildHashU64 >
}
impl<T: HashConsed, V> HConMap<T, V>
where T::Inner: Hash {
  /// An empty map of hashconsed things.
  #[inline]
  pub fn new() -> Self {
    HConMap {
      map: HashMap::with_hasher(BuildHashU64 {})
    }
  }
  /// An empty map of hashconsed things with a capacity.
  #[inline]
  pub fn with_capacity(capa: usize) -> Self {
    HConMap {
      map: HashMap::with_capacity_and_hasher(capa, BuildHashU64 {})
    }
  }
}
impl<T: HashConsed, V> Deref for HConMap<T, V> {
  type Target = HashMap<HConsed<T::Inner>, V, BuildHashU64> ;
  fn deref(& self) -> & Self::Target {
    & self.map
  }
}
impl<T: HashConsed, V> DerefMut for HConMap<T, V> {
  fn deref_mut(& mut self) -> & mut Self::Target {
    & mut self.map
  }
}


/// Optimal trivial hash for `usize`s and `u64`s. The former is used for
/// wrapped indices, the latter for hashconsed things.
///
/// **NEVER USE THIS MODULE DIRECTLY. ONLY THROUGH THE `wrap_usize` MACRO.**
///
/// This is kind of unsafe, in a way. The hasher will cause logic errors if
/// asked to hash anything else than what it was supposed to hash.
///
/// In `debug`, this is actually checked each time something is hashed. This
/// check is of course deactivated in `release`.
mod hash {
  use std::hash::{ Hasher, BuildHasher } ;

  /// Empty struct used to build `HashU64`.
  #[derive(Clone)]
  pub struct BuildHashU64 {}
  impl BuildHasher for BuildHashU64 {
    type Hasher = HashU64 ;
    fn build_hasher(& self) -> HashU64 {
      HashU64 { buf: [0 ; 8] }
    }
  }
  impl Default for BuildHashU64 {
    fn default() -> Self {
      BuildHashU64 {}
    }
  }

  /// Trivial hasher for `usize`. **This hasher is only for hashing `usize`s**.
  pub struct HashU64 {
    buf: [u8 ; 8]
  }
  impl HashU64 {
    /// Checks that a slice of bytes has the length of a `usize`. Only active
    /// in debug.
    #[cfg(debug)]
    #[inline(always)]
    fn test_bytes(bytes: & [u8]) {
      if bytes.len() != 8 {
        panic!(
          "[illegal] `HashU64::hash` \
          called with non-`u64` argument ({} bytes, expected {})",
          bytes.len(), 8
        )
      }
    }
    /// Checks that a slice of bytes has the length of a `usize`. Only active
    /// in debug.
    #[cfg( not(debug) )]
    #[inline(always)]
    fn test_bytes(_: & [u8]) {}
  }
  impl Hasher for HashU64 {
    fn finish(& self) -> u64 {
      unsafe {
        ::std::mem::transmute(self.buf)
      }
    }
    fn write(& mut self, bytes: & [u8]) {
      Self::test_bytes(bytes) ;
      for n in 0..8 {
        self.buf[n] = bytes[n]
      }
    }
  }
}