//! Efficient hash collections for hashconsed data.
//!
//! This module provide hash set and hash map types with simple hash functions for hashconsed types.
//! The hash of an hashconsed value is its unique identifier, multiplied by a large prime. This is
//! obviously extremely dangerous from a security point of view: these collections should **never**
//! be used for cryptographic purposes.
//!
//! Note that you can also use `BTreeMap` and `BTreeSet` on hashconsed types since they are totally
//! ordered.
//!
//! # Hashers
//!
//! This crate provides various hashers. The main discussion about this crate's hashers is in [PR 8
//! on github].
//!
//! [PR 8 on github]: https://github.com/AdrienChampion/hashconsing/pull/8 (repo's PR 8 on github)
//!
//! This module exposes hash-collections `HConSet` and `HConMap`, both parameterized by a hash
//! implementation. This module provides the following hashers, ordered from least efficient to most
//! efficient:
//!
//! - [`id_hash`](./hashers/id_hash): deterministic, inefficient, mostly here for legacy reasons;
//! - [`sip_hash`](./hashers/sip_hash): Rust's standard hasher, non-deterministic;
//! - [`p_hash`](./hashers/p_hash): deterministic and efficient, replaces the obsolete `id_hash`;
//! - [`a_hash`](./hashers/a_hash): non-deterministic and very efficient hasher, requires the
//!   activation of feature `with_ahash`.
//!
//! The default hasher for `HConSet` and `HConMap` is `p_hash`.
//!
//! This module exposes a dedicated sub-module for each of these hashers that re-exports `HConSet`
//! and `HConMap` while forcing the appropriate hasher.
//!
//!
//! # Usage
//!
//! > TL;DR You need to specify the hashconsed type when creating one of the collections in this
//! > module.
//!
//! There is a bit of internal gymnastic so that the type signatures of these collections are
//! natural. If `Term` is the hashconsed version of `RTerm`, then you want the type of the sets to
//! be the natural one, *e.g.* `HConSet<Term>`.
//!
//! However, since `Term` is really an alias for `HConsed<RTerm>`, then if we wanted to declare
//! `HConSet` as an alias for `HashSet` we would get `type HConSet<Inner> = HashSet< HConsed<Inner>
//! >` (omitting the custom hasher). That is, our sets would have type `HConSet<RTerm>`, which is
//! not very pretty. We could just define an alias though: `type TermSet = HConSet<RTerm>`, but it
//! turns out it's better to wrap the actual set in a `struct` anyway. Mostly to be able to define
//! `new` and `with_capacity` without relying on a trait (users would need to import) to do that.
//!
//! So actually `HConsed` types automatically implement the internal `trait HashConsed { type Inner;
//! }`. The sole purpose of this trait (currently) is to pass the inner type implicitly thanks to a
//! `T: HashConsed` bound. Rust's type inference does not seem to really like this, and struggles a
//! bit to infer the types at play. In practice, it means that you need to specify the type of the
//! hashconsed elements in your set/map.
//!
//! ```
//! use hashconsing::{*, hash_coll::default::*};
//!
//! #[derive(Hash, Clone, PartialEq, Eq)]
//! enum ActualTerm {
//!   Var(usize),
//!   Lam(Term),
//!   App(Term, Term)
//! }
//! type Term = HConsed<ActualTerm>;
//!
//! let mut consign = HConsign::empty();
//! assert_eq!(consign.len(), 0);
//!
//! let mut map: HConMap<Term,_> = HConMap::with_capacity(100);
//! let mut set: HConSet<Term> = HConSet::with_capacity(100);
//!
//! let (v1, v1_name) = (
//!   consign.mk( ActualTerm::Var(0) ), "v1"
//! );
//! assert_eq!(consign.len(), 1);
//! let prev = map.insert(v1.clone(), v1_name);
//! assert_eq!( prev, None );
//! let is_new = set.insert(v1.clone());
//! assert!( is_new );
//! ```
//!
//! The problem completely goes away if you redefine your set/map type, and is the recommended way
//! of using these collections.
//!
//! ```
//! use hashconsing::{*, hash_coll::*};
//!
//! #[derive(Hash, Clone, PartialEq, Eq)]
//! enum ActualTerm {
//!   Var(usize),
//!   Lam(Term),
//!   App(Term, Term)
//! }
//! type Term = HConsed<ActualTerm>;
//! type TermMap<T> = HConMap<Term, T>;
//! type TermSet = HConSet<Term>;
//!
//! let mut consign = HConsign::empty();
//! assert_eq!(consign.len(), 0);
//!
//! let mut map = TermMap::with_capacity(100);
//! let mut set = TermSet::with_capacity(100);
//!
//! let (v1, v1_name) = (
//!   consign.mk( ActualTerm::Var(0) ), "v1"
//! );
//! assert_eq!(consign.len(), 1);
//! let prev = map.insert(v1.clone(), v1_name);
//! assert_eq!( prev, None );
//! let is_new = set.insert(v1.clone());
//! assert!( is_new );
//! ```
//!
//! One can modify the hash use by term maps and sets, as well as the term map underlying a
//! consignment.
//!
//! ```
//! use hashconsing::{*, hash_coll::*};
//! use std::collections::hash_map::RandomState;
//!
//! #[derive(Hash,PartialEq,Eq,Clone)]
//! struct ActualSum(Vec<Sum>);
//! type Sum = HConsed<ActualSum>;
//!
//! consign! {
//!     /// Factory for terms. Uses the standard library's "SipHash".
//!     let factory = consign(37, RandomState::new()) for ActualSum;
//! }
//!
//! fn main() {
//!     // Map from terms. Uses the standard library's "SipHash".
//!     let map = HConMap::<Sum, usize, RandomState>::with_hasher(RandomState::new());
//! }
//! ```
use std::{
    collections::{HashMap, HashSet},
    hash::{BuildHasher, Hash, Hasher},
    ops::{Deref, DerefMut},
};

pub mod hashers;

/// Hash sets (maps) for (from) hconsed elements using [`ahash`].
///
/// > **NB:** this module is empty if the `"with_ahash"` feature is not active.
///
/// [ahash]: https://crates.io/crates/ahash (ahash on crates.io)
pub mod a_hash {
    /// Set of hashconsed elements.
    #[cfg(feature = "with_ahash")]
    pub type HConSet<T> = super::HConSet<T, super::hashers::a_hash::Builder>;
    /// Map from hashconsed elements.
    #[cfg(feature = "with_ahash")]
    pub type HConMap<K, V> = super::HConMap<K, V, super::hashers::a_hash::Builder>;
}
/// Hash sets (maps) for (from) hconsed elements using Rust's `std` hasher.
pub mod sip_hash {
    /// Set of hashconsed elements.
    pub type HConSet<T> = super::HConSet<T, super::hashers::sip_hash::Builder>;
    /// Map from hashconsed elements.
    pub type HConMap<K, V> = super::HConMap<K, V, super::hashers::sip_hash::Builder>;
}
/// Hash sets (maps) for (from) hconsed elements using [`p-hash`].
///
/// [`p-hash`]: https://github.com/AdrienChampion/hashconsing/pull/8 (p-hash in PR 8 on github)
pub mod p_hash {
    /// Set of hashconsed elements.
    pub type HConSet<T> = super::HConSet<T, super::hashers::p_hash::Builder>;
    /// Map from hashconsed elements.
    pub type HConMap<K, V> = super::HConMap<K, V, super::hashers::p_hash::Builder>;
}
/// Hash sets (maps) for (from) hconsed elements using [`id-hash`].
///
/// [`id-hash`]: https://github.com/AdrienChampion/hashconsing/pull/8 (p-hash in PR 8 on github)
pub mod id_hash {
    /// Set of hashconsed elements.
    pub type HConSet<T> = super::HConSet<T, super::hashers::id_hash::Builder>;
    /// Map from hashconsed elements.
    pub type HConMap<K, V> = super::HConMap<K, V, super::hashers::id_hash::Builder>;
}
/// Default hash sets (maps) for (from) hconsed elements.
///
/// Just a re-export of [`p_hash`](../p_hash/).
pub mod default {
    pub use super::p_hash::*;
}

use crate::{HConsed, HashConsed};

/// A hash set of hash-consed things.
///
/// Its second argument is a [`BuildHasher`] which should be used to construct the hash function
/// for this set. The default is an in-crate hash function which just multiplies the identifier of
/// the hash-consed type by a large prime.
#[derive(Clone, Debug)]
pub struct HConSet<T, S = self::hashers::p_hash::Builder>
where
    T: HashConsed,
    T::Inner: Eq + Hash,
{
    set: HashSet<HConsed<T::Inner>, S>,
}

impl<T, S> PartialEq for HConSet<T, S>
where
    T: HashConsed,
    T::Inner: Eq + Hash,
    S: BuildHasher,
{
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().zip(other.iter()).all(|(e_1, e_2)| e_1 == e_2)
    }
}

impl<T, S> Eq for HConSet<T, S>
where
    T: HashConsed,
    T::Inner: Eq + Hash,
    S: BuildHasher,
{
}

impl<T, S> Hash for HConSet<T, S>
where
    T: HashConsed,
    T::Inner: Eq + Hash,
    S: BuildHasher,
{
    fn hash<H>(&self, h: &mut H)
    where
        H: Hasher,
    {
        for elem in self {
            elem.hash(h)
        }
    }
}

impl<T, S> Default for HConSet<T, S>
where
    T: HashConsed,
    T::Inner: Eq + Hash,
    S: Default,
{
    fn default() -> Self {
        HConSet {
            set: HashSet::with_hasher(S::default()),
        }
    }
}

macro_rules! hashplement {
    {$(
        $(#[$meta:meta])*
        $hcoll_name:ident <
            $key_t_param:ident
            $(, $val_t_param:ident)?
        > for $hasher:ty ;
    )*} => {$(
        $(#[$meta])*
        impl<$key_t_param $(, $val_t_param)?> $hcoll_name<
            $key_t_param $(, $val_t_param)?, $hasher
        >
        where
            $key_t_param: HashConsed,
            $key_t_param::Inner: Eq + Hash,
        {
            /// Empty constructor.
            #[inline]
            pub fn new() -> Self {
                $hcoll_name::with_hasher(<$hasher>::new())
            }
            /// Empty constructor with capacity.
            pub fn with_capacity(capa: usize) -> Self {
                $hcoll_name::with_capacity_and_hasher(capa, <$hasher>::new())
            }
        }
    )*};
}

hashplement! {
    #[cfg(feature = "with_ahash")]
    HConSet<T> for self::hashers::a_hash::Builder;
    HConSet<T> for self::hashers::sip_hash::Builder;
    HConSet<T> for self::hashers::p_hash::Builder;
    HConSet<T> for self::hashers::id_hash::Builder;
}

impl<T, S> HConSet<T, S>
where
    T: HashConsed,
    T::Inner: Eq + Hash,
    S: BuildHasher,
{
    /// An empty set of hashconsed things, using a custom hash.
    ///
    /// See [`BuildHasher`] for the trait that `build_hasher` must implement, or
    /// [`HConMap::with_hasher`] for an example.
    #[inline]
    pub fn with_hasher(build_hasher: S) -> Self {
        HConSet {
            set: HashSet::with_hasher(build_hasher),
        }
    }
    /// An empty set of hashconsed things with a capacity and a custom hash.
    ///
    /// See [`BuildHasher`] for the trait that `build_hasher` must implement.
    #[inline]
    pub fn with_capacity_and_hasher(capa: usize, build_hasher: S) -> Self {
        HConSet {
            set: HashSet::with_capacity_and_hasher(capa, build_hasher),
        }
    }
}

impl<T, S> HConSet<T, S>
where
    T: HashConsed,
    T::Inner: Eq + Hash,
{
    /// An iterator visiting all elements.
    #[inline]
    pub fn iter(&self) -> ::std::collections::hash_set::Iter<HConsed<T::Inner>> {
        self.set.iter()
    }
}

impl<'a, T, S> IntoIterator for &'a HConSet<T, S>
where
    T: HashConsed,
    T::Inner: Hash + Eq,
{
    type Item = &'a HConsed<T::Inner>;
    type IntoIter = ::std::collections::hash_set::Iter<'a, HConsed<T::Inner>>;
    fn into_iter(self) -> Self::IntoIter {
        (&self.set).into_iter()
    }
}
impl<T, S> IntoIterator for HConSet<T, S>
where
    T: HashConsed,
    T::Inner: Hash + Eq,
{
    type Item = HConsed<T::Inner>;
    type IntoIter = ::std::collections::hash_set::IntoIter<HConsed<T::Inner>>;
    fn into_iter(self) -> Self::IntoIter {
        self.set.into_iter()
    }
}
impl<T, S> ::std::iter::FromIterator<HConsed<T::Inner>> for HConSet<T, S>
where
    T: HashConsed,
    T::Inner: Hash + Eq,
    S: BuildHasher + Default,
{
    fn from_iter<I: IntoIterator<Item = HConsed<T::Inner>>>(iter: I) -> Self {
        HConSet {
            set: HashSet::from_iter(iter),
        }
    }
}
impl<T, S> Deref for HConSet<T, S>
where
    T: HashConsed,
    T::Inner: Hash + Eq,
{
    type Target = HashSet<HConsed<T::Inner>, S>;
    fn deref(&self) -> &Self::Target {
        &self.set
    }
}
impl<T, S> DerefMut for HConSet<T, S>
where
    T: HashConsed,
    T::Inner: Hash + Eq,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.set
    }
}

impl<T, Src, S> From<Src> for HConSet<HConsed<T>, S>
where
    T: Hash + Eq,
    Src: Iterator<Item = HConsed<T>>,
    S: Default + BuildHasher,
{
    fn from(src: Src) -> Self {
        let mut set = HConSet::default();
        for elem in src {
            set.insert(elem);
        }
        set
    }
}

/// A hash map of hash-consed things.
///
/// Its second argument is a [`BuildHasher`] which should be used to construct the hash function
/// for this set. The default is an in-crate hash function which just multiplies the identifier of
/// the hash-consed type by a large prime.
#[derive(Clone, Debug)]
pub struct HConMap<T, V, S = self::hashers::p_hash::Builder>
where
    T: HashConsed,
    T::Inner: Hash + Eq,
{
    map: HashMap<HConsed<T::Inner>, V, S>,
}

impl<T, V, S> PartialEq for HConMap<T, V, S>
where
    T: HashConsed,
    T::Inner: Eq + Hash,
    V: Eq,
    S: BuildHasher,
{
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len()
            && self
                .iter()
                .zip(other.iter())
                .all(|((k_1, v_1), (k_2, v_2))| k_1 == k_2 && v_1 == v_2)
    }
}

impl<T, V, S> Eq for HConMap<T, V, S>
where
    T: HashConsed,
    T::Inner: Eq + Hash,
    V: Eq,
    S: BuildHasher,
{
}

impl<T, V, S> Hash for HConMap<T, V, S>
where
    T: HashConsed,
    T::Inner: Eq + Hash,
    V: Hash,
    S: BuildHasher,
{
    fn hash<H>(&self, h: &mut H)
    where
        H: Hasher,
    {
        for (key, value) in self {
            key.hash(h);
            value.hash(h)
        }
    }
}

impl<T, V, S> Default for HConMap<T, V, S>
where
    T: HashConsed,
    T::Inner: Eq + Hash,
    S: Default,
{
    fn default() -> Self {
        HConMap {
            map: HashMap::default(),
        }
    }
}

hashplement! {
    #[cfg(feature = "with_ahash")]
    HConMap<K, V> for self::hashers::a_hash::Builder;
    HConMap<K, V> for self::hashers::sip_hash::Builder;
    HConMap<K, V> for self::hashers::p_hash::Builder;
    HConMap<K, V> for self::hashers::id_hash::Builder;
}

impl<T, V, S> HConMap<T, V, S>
where
    T: HashConsed,
    T::Inner: Eq + Hash,
    S: BuildHasher,
{
    /// An empty map of hashconsed things, using a custom hash.
    ///
    /// See [`BuildHasher`] for the trait that `build_hasher` must implement.
    ///
    /// ## Example
    ///
    /// ```
    /// use std::collections::hash_map::RandomState;
    /// use hashconsing::{HConsed, hash_coll::HConMap};
    ///
    /// #[derive(Hash,PartialEq,Eq)]
    /// struct ActualSum(Vec<Sum>);
    /// type Sum = HConsed<ActualSum>;
    ///
    /// // Build map with standard library's hash builder (SipHash)
    /// let map = HConMap::<Sum, usize, _>::with_hasher(RandomState::new());
    /// ```
    #[inline]
    pub fn with_hasher(build_hasher: S) -> Self {
        HConMap {
            map: HashMap::with_hasher(build_hasher),
        }
    }
    /// An empty map of hashconsed things with a capacity and a custom hash.
    ///
    /// See [`BuildHasher`] for the trait that `build_hasher` must implement.
    #[inline]
    pub fn with_capacity_and_hasher(capa: usize, build_hasher: S) -> Self {
        HConMap {
            map: HashMap::with_capacity_and_hasher(capa, build_hasher),
        }
    }
}

impl<T: HashConsed, V, S> HConMap<T, V, S>
where
    T::Inner: Hash + Eq,
{
    /// An iterator visiting all elements.
    #[inline]
    pub fn iter(&self) -> ::std::collections::hash_map::Iter<HConsed<T::Inner>, V> {
        self.map.iter()
    }
    /// An iterator visiting all elements.
    #[inline]
    pub fn iter_mut(&mut self) -> ::std::collections::hash_map::IterMut<HConsed<T::Inner>, V> {
        self.map.iter_mut()
    }
}

impl<'a, T, V, S> IntoIterator for &'a HConMap<T, V, S>
where
    T: HashConsed,
    T::Inner: Hash + Eq,
{
    type Item = (&'a HConsed<T::Inner>, &'a V);
    type IntoIter = ::std::collections::hash_map::Iter<'a, HConsed<T::Inner>, V>;
    fn into_iter(self) -> Self::IntoIter {
        (&self.map).into_iter()
    }
}
impl<'a, T, V, S> IntoIterator for &'a mut HConMap<T, V, S>
where
    T: HashConsed,
    T::Inner: Hash + Eq,
{
    type Item = (&'a HConsed<T::Inner>, &'a mut V);
    type IntoIter = ::std::collections::hash_map::IterMut<'a, HConsed<T::Inner>, V>;
    fn into_iter(self) -> Self::IntoIter {
        (&mut self.map).into_iter()
    }
}
impl<T, V, S> IntoIterator for HConMap<T, V, S>
where
    T: HashConsed,
    T::Inner: Hash + Eq,
{
    type Item = (HConsed<T::Inner>, V);
    type IntoIter = ::std::collections::hash_map::IntoIter<HConsed<T::Inner>, V>;
    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }
}
impl<T, V, S> ::std::iter::FromIterator<(HConsed<T::Inner>, V)> for HConMap<T, V, S>
where
    T: HashConsed,
    T::Inner: Hash + Eq,
    S: Default + BuildHasher,
{
    fn from_iter<I: IntoIterator<Item = (HConsed<T::Inner>, V)>>(iter: I) -> Self {
        HConMap {
            map: HashMap::from_iter(iter),
        }
    }
}
impl<T: HashConsed, V, S> Deref for HConMap<T, V, S>
where
    T::Inner: Hash + Eq,
{
    type Target = HashMap<HConsed<T::Inner>, V, S>;
    fn deref(&self) -> &Self::Target {
        &self.map
    }
}
impl<T: HashConsed, V, S> DerefMut for HConMap<T, V, S>
where
    T::Inner: Hash + Eq,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.map
    }
}

impl<T, V, Src, S> From<Src> for HConMap<HConsed<T>, V, S>
where
    T: Hash + Eq,
    Src: Iterator<Item = (HConsed<T>, V)>,
    S: BuildHasher + Default,
{
    fn from(src: Src) -> Self {
        let mut set = HConMap::default();
        for (elem, value) in src {
            set.insert(elem, value);
        }
        set
    }
}

/// Simple hash for `usize`s and `u64`s---simply multiplies the number by a fixed 64-bit
/// prime. The former is used for wrapped indices, the latter for hashconsed things.
///
/// This is kind of unsafe, in a way. The hasher will cause logic errors if
/// asked to hash anything else than what it was supposed to hash.
///
/// In `debug`, this is actually checked each time something is hashed. This
/// check is of course deactivated in `release`.
mod hash {
    use std::hash::{BuildHasher, Hasher};

    /// Empty struct used to build `HashU64`.
    #[derive(Clone)]
    pub struct BuildHashU64 {}
    impl BuildHashU64 {
        #[allow(dead_code)]
        pub fn new() -> Self {
            Self {}
        }
    }
    impl BuildHasher for BuildHashU64 {
        type Hasher = HashU64;
        fn build_hasher(&self) -> HashU64 {
            HashU64 { buf: [0; 8] }
        }
    }
    impl Default for BuildHashU64 {
        fn default() -> Self {
            BuildHashU64 {}
        }
    }

    /// Simple hasher for `usize`. Multiplies the number by: `0xDA5DF7A7BD02F2C7u64`, a 64-bit
    /// prime. **This hasher is only for hashing `usize`s**.
    pub struct HashU64 {
        buf: [u8; 8],
    }
    impl HashU64 {
        /// Checks that a slice of bytes has the length of a `usize`. Only active
        /// in debug.
        #[cfg(debug)]
        #[inline(always)]
        fn test_bytes(bytes: &[u8]) {
            if bytes.len() != 8 {
                panic!(
                    "[illegal] `HashU64::hash` \
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
    impl Hasher for HashU64 {
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
