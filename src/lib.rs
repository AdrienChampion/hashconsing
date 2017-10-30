// See the LICENSE files at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

/*! Hash consing library.

It is a based on [Type-Safe Modular Hash-Consing](paper) by Filiâtre and
Conchon. It is slightly less efficient as uses Rust's `HashMap`s, not a custom
built structure.

Provides constant time comparison and perfect (maximal) sharing assuming only
one `HashConsign` is created for a given type. This assumption **must never be
falsified** unless you really, **really** know what you are doing.

Hash consed elements are immutable and therefore thread-safe: `HConsed`
implements `Send` and `Sync`.

The consign actually stores weak references to values. This ensures that values
are dropped once they are not used anymore.


# Collections with trivial hash function

This library provides two special collections: [`HConSet`][hcon set] and
[`HConMap`][hcon map]. They use the trivial hash function over hashconsed
values' unique identifier. [Read more.][coll mod]


# Example

Simple example for lambda calculus from [the paper][paper].

```
extern crate hashconsing ;

use std::fmt ;
use ::hashconsing::* ;


use self::ActualTerm::* ;

type Term = HConsed<ActualTerm> ;

#[derive(Hash, Clone, PartialEq, Eq)]
enum ActualTerm {
  Var(usize),
  Lam(Term),
  App(Term, Term)
}


impl fmt::Display for ActualTerm {
  fn fmt(& self, fmt: & mut fmt::Formatter) -> fmt::Result {
    match self {
      & Var(i) => write!(fmt, "v{}", i),
      & Lam(ref t) => write!(fmt, "({})", t.get()),
      & App(ref u, ref v) => write!(fmt, "{}.{}", u.get(), v.get()),
    }
  }
}

trait TermFactory {
  fn var(& mut self, v: usize) -> Term ;
  fn lam(& mut self, t: Term) -> Term ;
  fn app(& mut self, u: Term, v: Term) -> Term ;
}


impl TermFactory for HashConsign<ActualTerm> {
  fn var(& mut self, v: usize) -> Term { self.mk( Var(v) ) }
  fn lam(& mut self, t: Term) -> Term { self.mk( Lam(t) ) }
  fn app(& mut self, u: Term, v: Term) -> Term {
    self.mk( App(u, v) )
  }
}

pub fn main() {
  let mut consign = HashConsign::empty() ;
  assert_eq!(consign.len(), 0) ;

  let v = consign.var(0) ;
  assert_eq!(consign.len(), 1) ;

  let v2 = consign.var(3) ;
  assert_eq!(consign.len(), 2) ;

  let lam = consign.lam(v2) ;
  assert_eq!(consign.len(), 3) ;

  let v3 = consign.var(3) ;
  assert_eq!(consign.len(), 3) ;

  let lam2 = consign.lam(v3) ;
  assert_eq!(consign.len(), 3) ;

  let app = consign.app(lam2, v) ;
  assert_eq!(consign.len(), 4) ;
}
```

[paper]: http://dl.acm.org/citation.cfm?doid=1159876.1159880 (Type-safe modular hash-consing)
[hcon set]: coll/struct.HConSet.html (HConSet documentation)
[hcon map]: coll/struct.HConMap.html (HConMap documentation)
[coll mod]: coll/index.html (coll module documentation)
*/

use std::fmt ;
use std::sync::{ RwLock, Arc, Weak } ;
use std::marker::{ Send, Sync } ;
use std::collections::HashMap ;
use std::hash::{ Hash, Hasher } ;
use std::cmp::{
  PartialEq, Eq, PartialOrd, Ord, Ordering
} ;
use std::ops::Deref ;

pub mod coll ;

/// Internal trait used to recognize hashconsed things.
///
/// The only purpose of this trait (currently) is to simplify the type
/// signature of the collections of hashconsed things.
pub trait HashConsed {
  /// Elements stored inside a hashconsed value.
  type Inner ;
}

/// Stores a hash consed element and its hash in order to avoid recomputing it
/// every time.
pub struct HConsed<T> {
  /// The actual element.
  elm: Arc<T>,
  /// Unique identifier of the element.
  uid: u64,
}
impl<T> HashConsed for HConsed<T> {
  type Inner = T ;
}

impl<T> HConsed<T> {
  /// The inner element. Can also be accessed *via* dereferencing.
  #[inline(always)]
  pub fn get(& self) -> & T { self.elm.deref() }
  /// The unique identifier of the element.
  #[inline(always)]
  pub fn uid(& self) -> u64 { self.uid }
  /// Turns a hashconsed thing in a weak hashconsed thing.
  #[inline(always)]
  fn to_weak(& self) -> WHConsed<T> {
    WHConsed {
      elm: Arc::downgrade(& self.elm), uid: self.uid
    }
  }
}

impl<T: fmt::Debug> fmt::Debug for HConsed<T> {
  fn fmt(& self, fmt: & mut fmt::Formatter) -> fmt::Result {
    write!(fmt, "{:?}", self.elm)
  }
}

impl<T> Clone for HConsed<T> {
  fn clone(& self) -> Self {
    HConsed {
      elm: self.elm.clone(),
      uid: self.uid,
    }
  }
}

impl<T> PartialEq for HConsed<T> {
  #[inline(always)]
  fn eq(& self, rhs: & Self) -> bool {
    self.uid == rhs.uid
  }
}
impl<T> Eq for HConsed<T> {}
impl<T> PartialOrd for HConsed<T> {
  #[inline(always)]
  fn partial_cmp(& self, other: & Self) -> Option<Ordering> {
    self.uid.partial_cmp(& other.uid)
  }
}
impl<T> Ord for HConsed<T> {
  #[inline(always)]
  fn cmp(& self, other: & Self) -> Ordering {
    self.uid.cmp(& other.uid)
  }
}
impl<T: Hash> Hash for HConsed<T> {
  #[inline(always)]
  fn hash<H>(& self, state: & mut H) where H: Hasher {
    self.uid.hash(state)
  }
}

impl<T> Deref for HConsed<T> {
  type Target = T ;
  #[inline(always)]
  fn deref(& self) -> & T { self.elm.deref() }
}

unsafe impl<T> Sync for HConsed<T> {}
unsafe impl<T> Send for HConsed<T> {}

impl<T: fmt::Display> fmt::Display for HConsed<T> {
  #[inline(always)]
  fn fmt(& self, fmt: & mut fmt::Formatter) -> fmt::Result {
    self.elm.fmt(fmt)
  }
}

/// Weak version of `HConsed` (internal).
struct WHConsed<T> {
  /// The actual element.
  elm: Weak<T>,
  /// Unique identifier of the element.
  uid: u64,
}
impl<T> WHConsed<T> {
  /// Turns a weak hashconsed thing in a hashconsed thing.
  pub fn to_hconsed(& self) -> Option<HConsed<T>> {
    if let Some(arc) = self.elm.upgrade() {
      Some(
        HConsed {
          elm: arc, uid: self.uid
        }
      )
    } else { None }
  }
}

impl<T: fmt::Display> fmt::Display for WHConsed<T> {
  #[inline(always)]
  fn fmt(& self, fmt: & mut fmt::Formatter) -> fmt::Result {
    if let Some(arc) = self.elm.upgrade() { arc.fmt(fmt) } else {
      write!(fmt, "<freed>")
    }
  }
}

impl<T> Hash for WHConsed<T> {
  #[inline(always)]
  fn hash<H>(& self, state: & mut H) where H: Hasher {
    self.uid.hash(state)
  }
}

impl<T> PartialEq for WHConsed<T> {
  #[inline(always)]
  fn eq(& self, rhs: & Self) -> bool {
    self.uid == rhs.uid
  }
}
impl<T> Eq for WHConsed<T> {}
impl<T> PartialOrd for WHConsed<T> {
  #[inline(always)]
  fn partial_cmp(& self, other: & Self) -> Option<Ordering> {
    self.uid.partial_cmp(& other.uid)
  }
}
impl<T> Ord for WHConsed<T> {
  #[inline(always)]
  fn cmp(& self, other: & Self) -> Ordering {
    self.uid.cmp(& other.uid)
  }
}

/// The consign storing the actual hash consed elements as `HConsed`s.
pub struct HashConsign<T : Hash + Eq + Clone> {
  /// The actual hash consing table.
  table: HashMap<T, WHConsed<T>>,
  /// Counter for uids.
  count: u64,
}

impl<T : Hash + Eq + Clone> HashConsign<T> {
  /// Creates an empty consign.
  #[inline(always)]
  pub fn empty() -> Self {
    HashConsign {
      table: HashMap::new(),
      count: 0,
    }
  }

  /// Creates an empty consign with a capacity.
  #[inline(always)]
  pub fn with_capacity(capacity: usize) -> Self {
    HashConsign {
      table: HashMap::with_capacity(capacity),
      count: 0,
    }
  }

  /// Fold on the elements stored in the consign.
  #[inline]
  pub fn fold<Acc, F>(& self, f: F, mut init: Acc) -> Acc
  where F : Fn(Acc, HConsed<T>) -> Acc {
    for (_, weak) in self.table.iter() {
      if let Some(consed) = weak.to_hconsed() {
        init = f(init, consed)
      }
    }
    init
  }

  /// The number of elements stored, mostly for testing.
  #[inline(always)]
  pub fn len(& self) -> usize { self.table.len() }

  /// Inserts in the consign.
  ///
  /// One of the following must hold:
  ///
  /// - `self.table` is not defined at `key`
  /// - the weak ref in `self.table` at `key` returns `None` when upgraded.
  ///
  /// This is checked in `debug` but not `release`.
  #[inline]
  fn insert(& mut self, key: T, wconsed: WHConsed<T>) {
    let prev = self.table.insert(key, wconsed) ;
    debug_assert!(
      match prev {
        None => true,
        Some(prev) => prev.to_hconsed().is_none(),
      }
    )
  }

  /// Attempts to retrieve an *upgradable* value from the map.
  #[inline]
  fn get(& self, key: & T) -> Option<HConsed<T>> {
    if let Some(old) = self.table.get(key) {
      old.to_hconsed()
    } else { None }
  }
}

impl<T: Hash + Eq + Clone> fmt::Display for HashConsign<T>
where T: Hash + fmt::Display {
  fn fmt(& self, fmt: & mut fmt::Formatter) -> fmt::Result {
    try!( write!(fmt, "consign:") ) ;
    for (_, ref e) in self.table.iter() {
      try!( write!(fmt, "\n  | {}", e) ) ;
    }
    Ok(())
  }
}


/// HConsed element creation.
pub trait HConser<T: Hash>: Sized {
  /// Hashconses something and returns the hash consed version.
  ///
  /// Returns `true` iff the element
  ///
  /// - was not in the consign at all, or
  /// - was in the consign but it is not referenced (weak ref cannot be
  ///   upgraded.)
  fn mk_is_new(self, elm: T) -> (HConsed<T>, bool) ;
  /// Creates a HConsed element.
  fn mk(self, elm: T) -> HConsed<T> {
    self.mk_is_new(elm).0
  }
}
impl<'a, T: Hash + Eq + Clone> HConser<T> for & 'a mut HashConsign<T> {
  /// Hash conses something and returns the hash consed version.
  fn mk_is_new(self, elm: T) -> (HConsed<T>, bool) {
    // If the element is known and upgradable return it.
    if let Some(hconsed) = self.get(& elm) {
      debug_assert!(
        * hconsed.elm == elm
      ) ;
      return (hconsed.clone(), false)
    }
    // Otherwise build hconsed version.
    let hconsed = HConsed {
      elm: Arc::new( elm.clone() ),
      uid: self.count,
    } ;
    // Increment uid count.
    self.count += 1 ;
    // ...add weak version to the table...
    self.insert(elm, hconsed.to_weak()) ;
    // ...and return consed version.
    (hconsed, true)
  }
}
impl<
  'a, T: Hash + Eq + Clone
> HConser<T> for & 'a RwLock< HashConsign<T> > {
  /// If the element is already in the consign, only read access will be
  /// requested.
  fn mk_is_new(self, elm: T) -> (HConsed<T>, bool) {
    // Request read and check if element already exists.
    {
      let slf = self.read().unwrap() ;
      // If the element is known and upgradable return it.
      if let Some(hconsed) = slf.get(& elm) {
        debug_assert!(
          * hconsed.elm == elm
        ) ;
        return (hconsed, false)
      }
    } ;
    // Otherwise get mutable `self`.
    let mut slf = self.write().unwrap() ;

    // Someone might have inserted since we checked, check again.
    if let Some(hconsed) = slf.get(& elm) {
      debug_assert!(
        * hconsed.elm == elm
      ) ;
      return (hconsed, false)
    }

    // Otherwise build hconsed version.
    let hconsed = HConsed {
      elm: Arc::new( elm.clone() ),
      uid: slf.count,
    } ;
    // Increment uid count.
    slf.count += 1 ;
    // ...add weak version to the table...
    slf.insert(elm, hconsed.to_weak()) ;
    // ...and return consed version.
    (hconsed, true)
  }
}


#[cfg(test)]
mod example {

  use std::fmt ;
  use * ;
  use self::ActualTerm::* ;
  use coll::* ;

  type Term = HConsed<ActualTerm> ;

  #[derive(Hash, Clone, PartialEq, Eq)]
  enum ActualTerm {
    Var(usize),
    Lam(Term),
    App(Term, Term)
  }


  impl fmt::Display for ActualTerm {
    fn fmt(& self, fmt: & mut fmt::Formatter) -> fmt::Result {
      match self {
        & Var(i) => write!(fmt, "v{}", i),
        & Lam(ref t) => write!(fmt, "({})", t.get()),
        & App(ref u, ref v) => write!(fmt, "{}.{}", u.get(), v.get()),
      }
    }
  }

  trait TermFactory {
    fn var(& mut self, v: usize) -> Term ;
    fn lam(& mut self, t: Term) -> Term ;
    fn app(& mut self, u: Term, v: Term) -> Term ;
  }


  impl TermFactory for HashConsign<ActualTerm> {
    fn var(& mut self, v: usize) -> Term { self.mk( Var(v) ) }
    fn lam(& mut self, t: Term) -> Term { self.mk( Lam(t) ) }
    fn app(& mut self, u: Term, v: Term) -> Term {
      self.mk( App(u, v) )
    }
  }

  #[test]
  fn run() {
    let mut consign = HashConsign::empty() ;
    assert_eq!(consign.len(), 0) ;

    let mut map: HConMap<Term,_> = HConMap::with_capacity(100) ;
    let mut set: HConSet<Term> = HConSet::with_capacity(100) ;

    let (v1, v1_name) = (
      consign.var(0), "v1"
    ) ;
    println!("creating {}", v1) ;
    assert_eq!(consign.len(), 1) ;
    let prev = map.insert(v1.clone(), v1_name) ;
    assert_eq!( prev, None ) ;
    let is_new = set.insert(v1.clone()) ;
    assert!( is_new ) ;

    let (v2, v2_name) = (
      consign.var(3), "v2"
    ) ;
    println!("creating {}", v2) ;
    assert_eq!(consign.len(), 2) ;
    assert_ne!( v1.uid(), v2.uid() ) ;
    let prev = map.insert(v2.clone(), v2_name) ;
    assert_eq!( prev, None ) ;
    let is_new = set.insert(v2.clone()) ;
    assert!( is_new ) ;

    let (lam, lam_name) = (
      consign.lam( v2.clone() ), "lam"
    ) ;
    println!("creating {}", lam) ;
    assert_eq!(consign.len(), 3) ;
    assert_ne!( v1.uid(), lam.uid() ) ;
    assert_ne!( v2.uid(), lam.uid() ) ;
    let prev = map.insert(lam.clone(), lam_name) ;
    assert_eq!( prev, None ) ;
    let is_new = set.insert(lam.clone()) ;
    assert!( is_new ) ;

    let (v3, v3_name) = (
      consign.var(3), "v3"
    ) ;
    println!("creating {}", v3) ;
    assert_eq!(consign.len(), 3) ;
    assert_eq!( v2.uid(), v3.uid() ) ;
    let prev = map.insert(v3.clone(), v3_name) ;
    assert_eq!( prev, Some(v2_name) ) ;
    let is_new = set.insert(v3.clone()) ;
    assert!( ! is_new ) ;

    let (lam2, lam2_name) = (
      consign.lam( v3.clone() ), "lam2"
    ) ;
    println!("creating {}", lam2) ;
    assert_eq!(consign.len(), 3) ;
    assert_eq!( lam.uid(), lam2.uid() ) ;
    let prev = map.insert(lam2.clone(), lam2_name) ;
    assert_eq!( prev, Some(lam_name) ) ;
    let is_new = set.insert(lam2.clone()) ;
    assert!( ! is_new ) ;

    let (app, app_name) = (
      consign.app(lam2, v1), "app"
    ) ;
    println!("creating {}", app) ;
    assert_eq!(consign.len(), 4) ;
    let prev = map.insert(app.clone(), app_name) ;
    assert_eq!( prev, None ) ;
    let is_new = set.insert(app.clone()) ;
    assert!( is_new ) ;
  }
}