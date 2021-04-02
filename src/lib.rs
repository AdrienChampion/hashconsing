// See the LICENSE files at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Hash consing library.
//!
//! This library is based on [Type-Safe Modular Hash-Consing][paper] by Filiâtre and Conchon. It is
//! probably less efficient as uses Rust's `HashMap`s, not a custom built structure.
//!
//! If you are not familiar with hashconsing, see the [example](#example) below or read the paper.
//!
//! Provides constant time comparison and perfect (maximal) sharing assuming only one
//! consign/factory is created for a given type. This assumption **must never be falsified** unless
//! you really, **really** know what you are doing.
//!
//! Hash consed elements are immutable and therefore thread-safe: `HConsed` implements `Send` and
//! `Sync`.
//!
//! The consign actually stores weak references to values. This ensures that values are dropped
//! once they are not used anymore.
//!
//!
//! # Example
//!
//! Simple example for lambda calculus from [the paper][paper].
//!
//! Hashconsing consists in wrapping some tree-like datatype in an immutable container, which in
//! the case of `hashconsing` is [`HConsed`]. In this example we'll call the tree-like datatype
//! `ActualTerm` and the hashconsed version `Term`.
//!
//! A `Term` is created from an `ActualTerm` by a *factory*, which `hashconsing` calls a *consign*
//! (see [`HConsign`]). The functions for doing so are in the [`HashConsign` trait]. The idea is
//! that the consign is a map from actual terms to `Arc`s of hashconsed terms. When given an actual
//! term, the consign checks whether there's already a hashconsed term for it. If not, then it
//! creates one, memorizes it and returns that. Otherwise it clones the existing one. Hence subterm
//! sharing is maximal/perfect.
//!
//! A `HConsed<T>` is exactly two things: a unique identifier `uid` and an `Arc` to the real term
//! it represents. (Hence, cloning a hashconsed term is cheap.) This library guarantees that two
//! hashconsed terms refer to structurally identical real terms **iff** their `uid`s are equal.
//! Hence, equality checking is constant time.
//!
//! ```rust
//! extern crate hashconsing ;
//! use hashconsing::{ HConsed, HashConsign, HConsign } ;
//!
//! type Term = HConsed<ActualTerm> ;
//!
//! #[derive(Debug, Hash, Clone, PartialEq, Eq)]
//! enum ActualTerm {
//!     Var(usize),
//!     Lam(Term),
//!     App(Term, Term)
//! }
//! use self::ActualTerm::* ;
//!
//! fn main() {
//!     let mut factory: HConsign<ActualTerm> = HConsign::empty() ;
//!
//!     assert_eq! { factory.len(), 0 }
//!
//!     let v = factory.mk( Var(0) ) ;
//!     assert_eq! { factory.len(), 1 }
//!
//!     let v2 = factory.mk( Var(3) ) ;
//!     assert_eq! { factory.len(), 2 }
//!
//!     let lam = factory.mk(
//!         Lam( v2.clone() )
//!     ) ;
//!     assert_eq! { factory.len(), 3 }
//!
//!     let v3 = factory.mk( Var(3) ) ;
//!     // v2 is the same as v3: Var(3). Consign has not created anything new, and
//!     // v2 and v3 are conceptually the same term.
//!     assert_eq! { factory.len(), 3 }
//!     assert_eq! { v2.uid(), v3.uid() }
//!     assert_eq! { v2.get(), v3.get() }
//!     assert_eq! { v2,       v3       }
//!
//!     let lam2 = factory.mk( Lam(v3) ) ;
//!     // Not new either.
//!     assert_eq! { factory.len(), 3 }
//!     assert_eq! { lam.uid(), lam2.uid() }
//!     assert_eq! { lam.get(), lam2.get() }
//!     assert_eq! { lam,       lam2       }
//!
//!     let app = factory.mk( App(lam2, v) ) ;
//!     assert_eq! { factory.len(), 4 }
//! }
//! ```
//!
//! This library maintains the invariant stated above as long as you **never create two consigns
//! for the same type**.
//!
//! Users are free to use the consign however they see fit: one can create a factory directly as in
//! the example above, but passing it around everywhere it's needed is tedious. The author
//! recommends the following workflow instead. It relies on the [`consign`] macro which creates
//! a [lazy static] factory protected by a `RwLock` for thread-safety. The consign and the
//! constructors are wrapped in an appropriately named module. The consign is invisible and
//! creating terms is easy.
//!
//! ```rust
//! #[macro_use]
//! extern crate hashconsing ;
//!
//! pub mod term {
//!     use hashconsing::{ HConsed, HashConsign } ;
//!     pub type Term = HConsed<ActualTerm> ;
//!     #[derive(Debug, Hash, Clone, PartialEq, Eq)]
//!     pub enum ActualTerm {
//!         Var(usize),
//!         Lam(Term),
//!         App(Term, Term)
//!     }
//!
//!     consign! {
//!         /// Factory for terms.
//!         let factory = consign(37) for ActualTerm ;
//!     }
//!     pub fn var(v: usize) -> Term {
//!         factory.mk( ActualTerm::Var(v) )
//!     }
//!     pub fn lam(t: Term) -> Term {
//!         factory.mk( ActualTerm::Lam(t) )
//!     }
//!     pub fn app(t_1: Term, t_2: Term) -> Term {
//!         factory.mk( ActualTerm::App(t_1, t_2) )
//!     }
//! }
//!
//! fn main() {
//!     let v = term::var(0) ;
//!     let v2 = term::var(3) ;
//!     let lam = term::lam(v2) ;
//!     let v3 = term::var(3) ;
//!     let lam2 = term::lam(v3) ;
//!     let app = term::app(lam2, v) ;
//! }
//! ```
//!
//! Note that `HConsed<T>` `Deref`s to `T`, so you don't need to extend `HConsed<T>` using some
//! trait to add the functions you need. Just implement them for `T`. Functions taking an `& mut
//! self` won't work since `HConsed<T>` gives you access to `T` through an `Arc`.
//!
//! ```rust
//! # extern crate hashconsing ;
//! # pub mod term {
//! #     use hashconsing::* ;
//! #     pub type Term = HConsed<ActualTerm> ;
//! #     #[derive(Debug, Hash, Clone, PartialEq, Eq)]
//! #     pub enum ActualTerm {
//! #         Var(usize),
//! #         Lam(Term),
//! #         App(Term, Term)
//! #     }
//! #
//! #     consign! {
//! #         /// Factory for terms.
//! #         let factory = consign(37) for ActualTerm ;
//! #     }
//! #     pub fn var(v: usize) -> Term {
//! #         factory.mk( ActualTerm::Var(v) )
//! #     }
//! #     pub fn lam(t: Term) -> Term {
//! #         factory.mk( ActualTerm::Lam(t) )
//! #     }
//! #     pub fn app(t_1: Term, t_2: Term) -> Term {
//! #         factory.mk( ActualTerm::App(t_1, t_2) )
//! #     }
//! impl ::std::fmt::Display for ActualTerm {
//!     fn fmt(& self, fmt: & mut ::std::fmt::Formatter) -> ::std::fmt::Result {
//!         match self {
//!             ActualTerm::Var(i) => write!(fmt, "v{}", i),
//!             ActualTerm::Lam(t) => write!(fmt, "({})", t.get()),
//!             ActualTerm::App(u, v) => write!(
//!                 fmt, "{}.{}", u.get(), v.get()
//!             ),
//!         }
//!     }
//! }
//! # }
//! fn main() {
//!     let v = term::var(0) ;
//!     let v3 = term::var(3) ;
//!     let lam2 = term::lam(v3) ;
//!     let app = term::app(lam2, v) ;
//!     assert_eq! { & format!("{}", app), "(v3).v0" }
//! }
//! ```
//!
//! # Collections with trivial hash function
//!
//! This library provides two special collections: [`HConSet`] and [`HConMap`]. They use the
//! trivial hash function over hashconsed values' unique identifier. [Read more.][coll mod]
//!
//! Another way to have efficient sets/maps of/from hashconsed things is to use the `BTree` sets
//! and maps from the standard library.
//!
//! [paper]: http://dl.acm.org/citation.cfm?doid=1159876.1159880
//! (Type-safe modular hash-consing)
//! [`HConsed`]: trait.HashConsed.html (HConsed type)
//! [`HConSet`]: coll/struct.HConSet.html (HConSet documentation)
//! [`HConMap`]: coll/struct.HConMap.html (HConMap documentation)
//! [`HashConsign` trait]: trait.HashConsign.html (HashConsign trait)
//! [`HConsign`]: struct.HConsign.html (HConsign type)
//! [`consign`]: macro.consign.html (consign macro)
//! [coll mod]: coll/index.html (coll module documentation)
//! [lazy static]: https://crates.io/crates/lazy_static
//! (lazy_static library on crates.io)

use std::{
    cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd},
    collections::HashMap,
    fmt,
    hash::{Hash, Hasher},
    ops::Deref,
    sync::{Arc, RwLock, Weak},
};

pub extern crate lazy_static;

pub use lazy_static::lazy_static;

#[cfg(test)]
mod test;

/// Creates a lazy static consign.
///
/// The consign is protected by a `RwLock`.
///
/// Arguments:
/// - `$(#[$meta:meta])*` meta stuff, typically comments ;
/// - `$name:ident` name of the consign ;
/// - `$capa:expr` initial capacity when creating the consign ;
/// - `$hash_builder:expr` optional hash builder, an
///   implementation of [`std::hash::BuildHasher`] ;
/// - `$typ:typ,` type being hashconsed (the underlying type, not the
///     hashconsed one) ;
#[macro_export]
macro_rules! consign {
    (
        $(#[$meta:meta])*
        let $name:ident = consign($capa:expr) for $typ:ty ;
    ) => (
        $crate::lazy_static! {
            $(#[$meta])*
            static ref $name: ::std::sync::RwLock<
                $crate::HConsign<$typ>
            > = ::std::sync::RwLock::new(
                $crate::HConsign::with_capacity( $capa )
            );
        }
    );
    (
        $(#[$meta:meta])*
        let $name:ident = consign($capa:expr,$hash_builder:expr) for $typ:ty ;
    ) => (
        $crate::lazy_static! {
            $(#[$meta])*
            static ref $name: ::std::sync::RwLock<
                $crate::HConsign<$typ>
            > = ::std::sync::RwLock::new(
                $crate::HConsign::with_capacity_and_hasher( $capa, $hash_builder )
            );
        }
    );
}

pub mod coll;

/// Internal trait used to recognize hashconsed things.
///
/// The only purpose of this trait (currently) is to simplify the type
/// signature of the collections of hashconsed things.
pub trait HashConsed {
    /// Elements stored inside a hashconsed value.
    type Inner;
}

/// A hashconsed value.
pub struct HConsed<T> {
    /// The actual element.
    elm: Arc<T>,
    /// Unique identifier of the element.
    uid: u64,
}
impl<T> HashConsed for HConsed<T> {
    type Inner = T;
}

impl<T> HConsed<T> {
    /// The inner element. Can also be accessed *via* dereferencing.
    #[inline]
    pub fn get(&self) -> &T {
        self.elm.deref()
    }
    /// The unique identifier of the element.
    #[inline]
    pub fn uid(&self) -> u64 {
        self.uid
    }
    /// Turns a hashconsed thing in a weak hashconsed thing.
    #[inline]
    pub fn to_weak(&self) -> WHConsed<T> {
        WHConsed {
            elm: Arc::downgrade(&self.elm),
            uid: self.uid,
        }
    }

    /// Number of (strong) references to this term.
    pub fn arc_count(&self) -> usize {
        Arc::strong_count(&self.elm)
    }
}

impl<T: fmt::Debug> fmt::Debug for HConsed<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{:?}", self.elm)
    }
}

impl<T> Clone for HConsed<T> {
    fn clone(&self) -> Self {
        HConsed {
            elm: self.elm.clone(),
            uid: self.uid,
        }
    }
}

impl<T> PartialEq for HConsed<T> {
    #[inline]
    fn eq(&self, rhs: &Self) -> bool {
        self.uid == rhs.uid
    }
}
impl<T> Eq for HConsed<T> {}
impl<T> PartialOrd for HConsed<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.uid.partial_cmp(&other.uid)
    }
}
impl<T> Ord for HConsed<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.uid.cmp(&other.uid)
    }
}
impl<T: Hash> Hash for HConsed<T> {
    #[inline]
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.uid.hash(state)
    }
}

impl<T> Deref for HConsed<T> {
    type Target = T;
    #[inline]
    fn deref(&self) -> &T {
        self.elm.deref()
    }
}

impl<T: fmt::Display> fmt::Display for HConsed<T> {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.elm.fmt(fmt)
    }
}

/// Weak version of `HConsed` (internal).
pub struct WHConsed<T> {
    /// The actual element.
    elm: Weak<T>,
    /// Unique identifier of the element.
    uid: u64,
}
impl<T> WHConsed<T> {
    /// Turns a weak hashconsed thing in a hashconsed thing.
    pub fn to_hconsed(&self) -> Option<HConsed<T>> {
        if let Some(arc) = self.elm.upgrade() {
            Some(HConsed {
                elm: arc,
                uid: self.uid,
            })
        } else {
            None
        }
    }
}

impl<T: fmt::Display> fmt::Display for WHConsed<T> {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if let Some(arc) = self.elm.upgrade() {
            arc.fmt(fmt)
        } else {
            write!(fmt, "<freed>")
        }
    }
}

impl<T> Hash for WHConsed<T> {
    #[inline]
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.uid.hash(state)
    }
}

impl<T> PartialEq for WHConsed<T> {
    #[inline]
    fn eq(&self, rhs: &Self) -> bool {
        self.uid == rhs.uid
    }
}
impl<T> Eq for WHConsed<T> {}
impl<T> PartialOrd for WHConsed<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.uid.partial_cmp(&other.uid)
    }
}
impl<T> Ord for WHConsed<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.uid.cmp(&other.uid)
    }
}

/// The consign storing the actual hash consed elements as `HConsed`s.
pub struct HConsign<T: Hash + Eq + Clone> {
    /// The actual hash consing table.
    table: HashMap<T, WHConsed<T>>,
    /// Counter for uids.
    count: u64,
}

impl<T: Hash + Eq + Clone> HConsign<T> {
    /// Creates an empty consign.
    #[inline]
    pub fn empty() -> Self {
        HConsign {
            table: HashMap::new(),
            count: 0,
        }
    }

    /// Creates an empty consign with a capacity.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        HConsign {
            table: HashMap::with_capacity(capacity),
            count: 0,
        }
    }

    /// Fold on the elements stored in the consign.
    #[inline]
    pub fn fold<Acc, F>(&self, mut init: Acc, mut f: F) -> Acc
    where
        F: FnMut(Acc, HConsed<T>) -> Acc,
    {
        for weak in self.table.values() {
            if let Some(consed) = weak.to_hconsed() {
                init = f(init, consed)
            }
        }
        init
    }

    /// Fold on the elements stored in the consign, result version.
    #[inline]
    pub fn fold_res<Acc, F, E>(&self, mut init: Acc, mut f: F) -> Result<Acc, E>
    where
        F: FnMut(Acc, HConsed<T>) -> Result<Acc, E>,
    {
        for weak in self.table.values() {
            if let Some(consed) = weak.to_hconsed() {
                init = f(init, consed)?
            }
        }
        Ok(init)
    }

    /// The number of elements stored, mostly for testing.
    #[inline]
    pub fn len(&self) -> usize {
        self.table.len()
    }
    /// Capacity of the underlying hashtable, mostly for testing.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.table.capacity()
    }

    /// True if the consign is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.table.is_empty()
    }

    /// Inserts in the consign.
    ///
    /// One of the following must hold:
    ///
    /// - `self.table` is not defined at `key`
    /// - the weak ref in `self.table` at `key` returns `None` when upgraded.
    ///
    /// This is checked in `debug` but not `release`.
    #[inline]
    fn insert(&mut self, key: T, wconsed: WHConsed<T>) {
        let prev = self.table.insert(key, wconsed);
        debug_assert!(match prev {
            None => true,
            Some(prev) => prev.to_hconsed().is_none(),
        })
    }

    /// Attempts to retrieve an *upgradable* value from the map.
    #[inline]
    fn get(&self, key: &T) -> Option<HConsed<T>> {
        if let Some(old) = self.table.get(key) {
            old.to_hconsed()
        } else {
            None
        }
    }
}

impl<T: Hash + Eq + Clone> fmt::Display for HConsign<T>
where
    T: Hash + fmt::Display,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "consign:")?;
        for e in self.table.values() {
            write!(fmt, "\n  | {}", e)?;
        }
        Ok(())
    }
}

/// HConsed element creation.
///
/// Implemented *via* a trait to be able to extend `RwLock` for lazy static
/// consigns.
pub trait HashConsign<T: Hash>: Sized {
    /// Hashconses something and returns the hash consed version.
    ///
    /// Returns `true` iff the element
    ///
    /// - was not in the consign at all, or
    /// - was in the consign but it is not referenced (weak ref cannot be
    ///   upgraded.)
    fn mk_is_new(self, elm: T) -> (HConsed<T>, bool);

    /// Creates a HConsed element.
    fn mk(self, elm: T) -> HConsed<T> {
        self.mk_is_new(elm).0
    }

    /// Removes *dead* elements from the consign.
    ///
    /// An element is *dead* if it is not being referenced outside of the consign, meaning it is not
    /// reachable anymore.
    fn collect(self);

    /// Shrinks the capacity of the consign as much as possible.
    fn shrink_to_fit(self);

    /// Calls [`collect`](#tymethod.collect) and [`shrink_to_fit`](#tymethod.shrink_to_fit).
    fn collect_to_fit(self);

    /// Reserves capacity for at least `additional` more elements.
    fn reserve(self, additional: usize);
}
impl<'a, T: Hash + Eq + Clone> HashConsign<T> for &'a mut HConsign<T> {
    fn mk_is_new(self, elm: T) -> (HConsed<T>, bool) {
        // If the element is known and upgradable return it.
        if let Some(hconsed) = self.get(&elm) {
            debug_assert!(*hconsed.elm == elm);
            return (hconsed.clone(), false);
        }
        // Otherwise build hconsed version.
        let hconsed = HConsed {
            elm: Arc::new(elm.clone()),
            uid: self.count,
        };
        // Increment uid count.
        self.count += 1;
        // ...add weak version to the table...
        self.insert(elm, hconsed.to_weak());
        // ...and return consed version.
        (hconsed, true)
    }

    fn collect(self) {
        let mut old_len = self.table.len() + 1;
        let mut max_uid = None;
        while old_len != self.table.len() {
            old_len = self.table.len();
            max_uid = None;

            self.table.retain(|_key, val| {
                if val.elm.strong_count() > 0 {
                    let max = max_uid.get_or_insert(val.uid);
                    if *max < val.uid {
                        *max = val.uid
                    }
                    true
                } else {
                    false
                }
            });
        }
        if self.table.is_empty() {
            self.count = 0
        } else if let Some(max) = max_uid {
            self.count = max + 1
        }
    }

    fn shrink_to_fit(self) {
        self.table.shrink_to_fit()
    }

    fn collect_to_fit(self) {
        self.collect();
        self.shrink_to_fit();
    }

    fn reserve(self, additional: usize) {
        self.table.reserve(additional)
    }
}

/// Helper macro to get a read or write version of a locked consign.
macro_rules! get {
    { read on $consign:expr } => {
        get! { @expect $consign.read() }
    };
    { write on $consign:expr } => {
        get! { @expect $consign.write() }
    };
    { @expect $e:expr } => {
        $e.expect("[hashconsing] global consign is poisoned")
    };
}

impl<'a, T: Hash + Eq + Clone> HashConsign<T> for &'a RwLock<HConsign<T>> {
    /// If the element is already in the consign, only read access will be
    /// requested.
    fn mk_is_new(self, elm: T) -> (HConsed<T>, bool) {
        // Request read and check if element already exists.
        {
            let slf = get!(read on self);
            // If the element is known and upgradable return it.
            if let Some(hconsed) = slf.get(&elm) {
                debug_assert!(*hconsed.elm == elm);
                return (hconsed, false);
            }
        };
        // Otherwise get mutable `self`.
        let mut slf = get!(write on self);

        // Someone might have inserted since we checked, check again.
        if let Some(hconsed) = slf.get(&elm) {
            debug_assert!(*hconsed.elm == elm);
            return (hconsed, false);
        }

        // Otherwise build hconsed version.
        let hconsed = HConsed {
            elm: Arc::new(elm.clone()),
            uid: slf.count,
        };
        // Increment uid count.
        slf.count += 1;
        // ...add weak version to the table...
        slf.insert(elm, hconsed.to_weak());
        // ...and return consed version.
        (hconsed, true)
    }

    fn collect(self) {
        get!(write on self).collect()
    }
    fn shrink_to_fit(self) {
        get!(write on self).shrink_to_fit()
    }
    fn collect_to_fit(self) {
        get!(write on self).collect_to_fit()
    }
    fn reserve(self, additional: usize) {
        get!(write on self).reserve(additional)
    }
}
