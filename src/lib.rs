// Copyright 2015 Adrien Champion. See the COPYRIGHT file at the top-level
// directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

/*! Hash consing library.

Straightforward implementation of the [paper by Filiâtre and Conchon][paper].

Provides constant time comparison and perfect (maximal) sharing assuming only
one `HashConsign` is created for a given type. This assumption **must never**
be falsified unless you really know what you are doing.

Hash consed elements are immutable and therefore thread-safe: `HConsed`
implements `Send` and `Sync`.


## TODO

Ideally we'd like to use weak references in the consign but they're currently
feature-gated. Waiting for dust to settle.

# Example

Simple example for lambda calculus from [the paper][paper].

```
extern crate hashconsing ;

use std::fmt ;
use ::hashconsing::* ;


use self::ActualTerm::* ;

type Term = HConsed<ActualTerm> ;

#[derive(Hash)]
enum ActualTerm {
  Var(usize),
  Lam(Term),
  App(Term, Term)
}

impl PartialEq for ActualTerm {
  fn eq(& self, rhs: & Self) -> bool {
    match (self, rhs) {
      (& Var(i), & Var(j)) =>
        i == j,
      (& Lam(ref t1), & Lam(ref t2)) =>
        t1.hkey() == t2.hkey(),
      (& App(ref u1, ref v1), & App(ref u2, ref v2)) =>
        u1.hkey() == u2.hkey() && v1.hkey() == v2.hkey(),
      _ => false
    }
  }
}
impl Eq for ActualTerm {}


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
*/

use std::fmt ;
use std::sync::{ RwLock, Arc } ;
use std::marker::{ Send, Sync } ;
use std::default::Default ;
use std::collections::HashMap ;
use std::collections::hash_map::Iter ;
use std::hash::SipHasher ;
use std::hash::{ Hash, Hasher } ;
use std::cmp::{
  PartialEq, Eq, PartialOrd, Ord, Ordering
} ;
use std::ops::Deref ;
use std::clone::Clone ;

/// Stores a hash consed element and its hash in order to avoid recomputing it
/// every time.
pub struct HConsed<T> {
  /// The actual element.
  elm: Arc<T>,
  /// The hash key of the element.
  hkey: u64,
  /// Unique identifier of the element.
  uid: u64,
}

impl<T> HConsed<T> {
  /// The element hash consed. Can also be accessed via dereferencing.
  #[inline(always)]
  pub fn get(& self) -> & T { self.elm.deref() }
  /// The hash key of the element.
  #[inline(always)]
  pub fn hkey(& self) -> u64 { self.hkey }
  /// The hash key of the element.
  #[inline(always)]
  pub fn uid(& self) -> u64 { self.uid }
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
      hkey: self.hkey,
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
impl<T> Hash for HConsed<T> {
  #[inline(always)]
  fn hash<H>(& self, state: & mut H) where H: Hasher {
    self.hkey.hash(state)
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

/// The consign storing the actual hash consed elements as `HConsed`s.
pub struct HashConsign<
  T : Hash, H: Hasher = SipHasher
> {
  /// The actual hash consing table.
  table: HashMap<u64, HConsed<T>>,
  /// Counter for uids.
  count: u64,
  /// Hasher.
  hasher: H,
}

impl<
  T : Hash, H: Hasher + Default + Clone
> HashConsign<T, H> {
  /// Creates an empty consign.
  #[inline(always)]
  pub fn empty() -> Self {
    HashConsign {
      table: HashMap::new(),
      count: 0,
      hasher: H::default(),
    }
  }

  /// Creates an empty consign with a capacity.
  #[inline(always)]
  pub fn with_capacity(capacity: usize) -> Self {
    HashConsign {
      table: HashMap::with_capacity(capacity),
      count: 0,
      hasher: H::default(),
    }
  }

  /// Iterator on the elements stored in the consign.
  #[inline]
  pub fn iter(& self) -> Iter<u64, HConsed<T>> {
    self.table.iter()
  }

  /// The number of elements stored, mostly for testing.
  #[inline(always)]
  pub fn len(& self) -> usize { self.table.len() }

  /// Hash of an element.
  #[inline(always)]
  fn hash(& self, elm: & T) -> u64 {
    let mut hasher = self.hasher.clone() ;
    elm.hash(& mut hasher) ;
    hasher.finish()
  }

  /// Inserts in the consign.
  #[inline(always)]
  fn insert(& mut self, hkey: u64, consed: HConsed<T>) {
    let prev = self.table.insert(hkey, consed) ;
    debug_assert!( prev.is_none() )
  }
}

impl<T> fmt::Display for HashConsign<T>
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
pub trait HConser<T: Hash> {
  /// Creates a HConsed element.
  fn mk(self, elm: T) -> HConsed<T> ;
}
impl<
  'a, T: Hash, H: Hasher + Default + Clone
> HConser<T> for & 'a mut HashConsign<T, H> {
  /// Hash conses something and returns the hash consed version.
  fn mk(self, elm: T) -> HConsed<T> {
    let hkey = self.hash(& elm) ;
    // If the element is known return it.
    if let Some(consed) = self.table.get(& hkey) { return consed.clone() } ;
    // Otherwise build the hash consed version...
    let uid = self.count ;
    self.count += 1 ;
    let consed = HConsed {
      elm: Arc::new(elm), hkey: hkey, uid: uid
    } ;
    // ...add it to the table...
    self.insert(hkey, consed.clone()) ;
    // ...and return it.
    consed
  }
}
impl<
  'a, T: Hash, H: Hasher + Default + Clone
> HConser<T> for & 'a RwLock< HashConsign<T, H> > {
  /// Hash conses something and returns the hash consed version.
  fn mk(self, elm: T) -> HConsed<T> {
    let hkey = {
      let slf = self.read().unwrap() ;
      let hkey = slf.hash(& elm) ;
      // If the element is known return it.
      if let Some(consed) = slf.table.get(& hkey) { return consed.clone() } ;
      hkey
    } ;
    // Otherwise build the hash consed version...
    let mut conser = self.write().unwrap() ;
    let uid = conser.count ;
    conser.count += 1 ;
    let consed = HConsed {
      elm: Arc::new(elm), hkey: hkey, uid: uid
    } ;
    // ...add it to the table...
    conser.insert(hkey, consed.clone()) ;
    // ...and return it.
    consed
  }
}