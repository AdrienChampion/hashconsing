/// Efficient collections for hashconsed data.
/// 
/// This module provide hash set and hash map types with trivial hash functions
/// for hashconsed types. The hash of an hashconsed value is its unique
/// identifier, verbatim. This is obviously extremely dangerous from a security
/// point of view: these collections should **never** be used for cryptographic
/// purposes.
/// 
/// 
/// # Usage
/// 
/// > TL;DR You need to specify the hashconsed type when creating one of the
/// > collections in this module.
/// 
/// There is a bit of internal gymnastic so that the type signatures of these
/// collections are natural. If `Term` is the hashconsed version of `RTerm`,
/// then you want the type of the sets to be the natural one, *e.g.*
/// `HConSet<Term>`.
/// 
/// However, since `Term` is really an alias for `HConsed<RTerm>`, then if we
/// wanted to declare `HConSet` as an alias for `HashSet` we would get `type
/// HConSet<Inner> = HashSet< HConsed<Inner> >` (omitting the custom hasher).
/// That is, our sets would have type `HConSet<RTerm>`, which is not very
/// pretty. We could just define an alias though: `type TermSet =
/// HConSet<RTerm>`, but it turns out it's better to wrap the actual set in a
/// `struct` anyway. Mostly to be able to define `new` and `with_capacity`
/// without relying on a trait (users would need to import) to do that.
/// 
/// So actually `HConsed` types automatically implement the internal `trait
/// HashConsed { type Inner ; }`. The sole purpose of this trait (currently) is
/// to pass the inner type implicitly thanks to a `T: HashConsed` bound. Rust's
/// type inference does not seem to really like this, and struggles a bit to
/// infer the types at play. In practice, it means that you need to specify the
/// type of the hashconsed elements in your set/map.
/// 
/// ```
/// use hashconsing::* ;
/// use hashconsing::coll::* ;
/// 
/// #[derive(Hash, Clone, PartialEq, Eq)]
/// enum ActualTerm {
///   Var(usize),
///   Lam(Term),
///   App(Term, Term)
/// }
/// type Term = HConsed<ActualTerm> ;
/// 
/// let mut consign = HashConsign::empty() ;
/// assert_eq!(consign.len(), 0) ;
/// 
/// let mut map: HConMap<Term,_> = HConMap::with_capacity(100) ;
/// let mut set: HConSet<Term> = HConSet::with_capacity(100) ;
/// 
/// let (v1, v1_name) = (
///   consign.mk( ActualTerm::Var(0) ), "v1"
/// ) ;
/// assert_eq!(consign.len(), 1) ;
/// let prev = map.insert(v1.clone(), v1_name) ;
/// assert_eq!( prev, None ) ;
/// let is_new = set.insert(v1.clone()) ;
/// assert!( is_new ) ;
/// ```

use std::collections::{ HashSet, HashMap } ;
use std::ops::{ Deref, DerefMut } ;
use std::hash::{ Hash, Hasher } ;

use { HConsed, HashConsed } ;
use self::hash::BuildHashU64 ;

/// A hash set of hash-consed things with trivial hashing.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HConSet<T>
where T: HashConsed, T::Inner: Eq + Hash {
  set: HashSet< HConsed<T::Inner>, BuildHashU64 >
}
impl<T> Hash for HConSet<T>
where T: HashConsed, T::Inner: Eq + Hash {
  fn hash<H>(& self, h: & mut H)
  where H: Hasher {
    for elem in self {
      elem.hash(h)
    }
  }
}
impl<T> Default for HConSet<T>
where T: HashConsed, T::Inner: Eq + Hash {
  fn default() -> Self {
    HConSet { set: HashSet::default() }
  }
}
impl<T> HConSet<T>
where T: HashConsed, T::Inner: Eq + Hash {
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
  /// An iterator visiting all elements.
  #[inline]
  pub fn iter(& self) -> ::std::collections::hash_set::Iter<
    HConsed<T::Inner>
  > {
    self.set.iter()
  }
}
impl<'a, T> IntoIterator for & 'a HConSet<T>
where T: HashConsed, T::Inner: Hash + Eq {
  type Item = & 'a HConsed<T::Inner> ;
  type IntoIter = ::std::collections::hash_set::Iter<'a, HConsed<T::Inner>> ;
  fn into_iter(self) -> Self::IntoIter {
    (& self.set).into_iter()
  }
}
impl<T> IntoIterator for HConSet<T>
where T: HashConsed, T::Inner: Hash + Eq {
  type Item = HConsed<T::Inner> ;
  type IntoIter = ::std::collections::hash_set::IntoIter<HConsed<T::Inner>> ;
  fn into_iter(self) -> Self::IntoIter {
    self.set.into_iter()
  }
}
impl<T> ::std::iter::FromIterator<HConsed<T::Inner>> for HConSet<T>
where T: HashConsed, T::Inner: Hash + Eq {
  fn from_iter<I: IntoIterator<Item = HConsed<T::Inner>>>(iter: I) -> Self {
    HConSet {
      set: HashSet::from_iter(iter)
    }
  }
}
impl<T> Deref for HConSet<T>
where T: HashConsed, T::Inner: Hash + Eq {
  type Target = HashSet<HConsed<T::Inner>, BuildHashU64> ;
  fn deref(& self) -> & Self::Target {
    & self.set
  }
}
impl<T> DerefMut for HConSet<T>
where T: HashConsed, T::Inner: Hash + Eq {
  fn deref_mut(& mut self) -> & mut Self::Target {
    & mut self.set
  }
}

/// A hash map of hash-consed things with trivial hashing.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HConMap<T: HashConsed, V>
where T::Inner: Hash + Eq {
  map: HashMap< HConsed<T::Inner>, V, BuildHashU64 >
}
impl<T: HashConsed, V> HConMap<T, V>
where T::Inner: Hash + Eq {
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
  /// An iterator visiting all elements.
  #[inline]
  pub fn iter(& self) -> ::std::collections::hash_map::Iter<
    HConsed<T::Inner>, V
  > {
    self.map.iter()
  }
  /// An iterator visiting all elements.
  #[inline]
  pub fn iter_mut(& mut self) -> ::std::collections::hash_map::IterMut<
    HConsed<T::Inner>, V
  > {
    self.map.iter_mut()
  }
}
impl<'a, T, V> IntoIterator for & 'a HConMap<T, V>
where T: HashConsed, T::Inner: Hash + Eq {
  type Item = (& 'a HConsed<T::Inner>, & 'a V) ;
  type IntoIter = ::std::collections::hash_map::Iter<
    'a, HConsed<T::Inner>, V
  > ;
  fn into_iter(self) -> Self::IntoIter {
    (& self.map).into_iter()
  }
}
impl<'a, T, V> IntoIterator for & 'a mut HConMap<T, V>
where T: HashConsed, T::Inner: Hash + Eq {
  type Item = (& 'a HConsed<T::Inner>, & 'a mut V) ;
  type IntoIter = ::std::collections::hash_map::IterMut<
    'a, HConsed<T::Inner>, V
  > ;
  fn into_iter(self) -> Self::IntoIter {
    (& mut self.map).into_iter()
  }
}
impl<T, V> IntoIterator for HConMap<T, V>
where T: HashConsed, T::Inner: Hash + Eq {
  type Item = (HConsed<T::Inner>, V) ;
  type IntoIter = ::std::collections::hash_map::IntoIter<
    HConsed<T::Inner>, V
  > ;
  fn into_iter(self) -> Self::IntoIter {
    self.map.into_iter()
  }
}
impl<T, V> ::std::iter::FromIterator<(HConsed<T::Inner>, V)> for HConMap<T,V>
where T: HashConsed, T::Inner: Hash + Eq {
  fn from_iter<
    I: IntoIterator<Item = (HConsed<T::Inner>, V)>
  >(iter: I) -> Self {
    HConMap {
      map: HashMap::from_iter(iter)
    }
  }
}
impl<T: HashConsed, V> Deref for HConMap<T, V>
where T::Inner: Hash + Eq {
  type Target = HashMap<HConsed<T::Inner>, V, BuildHashU64> ;
  fn deref(& self) -> & Self::Target {
    & self.map
  }
}
impl<T: HashConsed, V> DerefMut for HConMap<T, V>
where T::Inner: Hash + Eq {
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