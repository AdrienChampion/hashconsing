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

## Concurrent version

```
extern crate hashconsing ;

use std::thread ;
use std::sync::{ Arc, mpsc, Mutex } ;
use std::fmt ;

use hashconsing::* ;

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
  fn var(& self, v: usize) -> Term ;
  fn lam(& self, t: Term) -> Term ;
  fn app(& self, u: Term, v: Term) -> Term ;
  fn len(& self) -> usize ;
}


impl TermFactory for Arc<Mutex<HashConsign<ActualTerm>>> {
  fn var(& self, v: usize) -> Term { self.lock().unwrap().mk( Var(v) ) }
  fn lam(& self, t: Term) -> Term { self.lock().unwrap().mk( Lam(t) ) }
  fn app(& self, u: Term, v: Term) -> Term {
    self.lock().unwrap().mk( App(u, v) )
  }
  fn len(& self) -> usize { self.lock().unwrap().len() }
}


pub fn main() {
  let thread_count = 4 ;

  let consign = Arc::new( Mutex::new(HashConsign::empty()) ) ;
  assert_eq!(consign.len(), 0) ;

  // Master to slaves channel.
  let (tx, rx) = mpsc::channel() ;
  // Slave to slave channel.
  let (ts, rs) = mpsc::channel() ;

  let mut bla = consign.var(0) ;

  for i in 1..(thread_count + 1) {
    let (consign, tx) = (consign.clone(), tx.clone()) ;
    let ts = ts.clone() ;

    thread::spawn(move || {

      let v = consign.var(0) ;

      let v2 = consign.var(3) ;

      let lam = consign.lam(v2) ;
      if i == 1 { ts.send(lam.clone()).unwrap() ; () } ;

      let v3 = consign.var(3) ;
      if i == 2 { ts.send(v3.clone()).unwrap() ; () } ;

      let lam2 = consign.lam(v3) ;
      if i == 3 { ts.send(lam2.clone()).unwrap() ; () } ;

      let app = consign.app(lam2, v) ;
      if i == 4 { ts.send(app).unwrap() ; () } ;

      tx.send(i)
    }) ;
  }

  for _ in 0..thread_count {
    match rs.recv() {
      Ok(t) => bla = consign.app(t, bla),
      Err(e) => panic!("error {}.", e),
    }
    match rx.recv() {
      Ok(i) => println!("| Thread {} is done.", i),
      Err(e) => panic!("error {}.", e),
    }
  }
  assert_eq!(consign.len(), 7) ;
}
```

[paper]: http://dl.acm.org/citation.cfm?doid=1159876.1159880 (Type-safe modular hash-consing)
*/

use std::fmt ;
use std::sync::Arc ;
use std::marker::{ Send, Sync } ;
use std::collections::HashMap ;
use std::hash::SipHasher ;
use std::hash::{ Hash, Hasher } ;
use std::cmp::{
  PartialEq, Eq, PartialOrd, Ord, Ordering
} ;
use std::ops::Deref ;
use std::clone::Clone ;

/** Stores a hash consed element and its hash in order to avoid recomputing it
  every time. */
pub struct HConsed<T> {
  /** The actual element. */
  elm: Arc<T>,
  /** The hash key of the element. */
  hkey: u64,
}

impl<T> HConsed<T> {
  /** The element hash consed. Can also be accessed via dereferencing. */
  #[inline(always)]
  pub fn get(& self) -> & T { self.elm.deref() }
  /** The hash key of the element. */
  #[inline(always)]
  pub fn hkey(& self) -> u64 { self.hkey }
}

impl<T> Clone for HConsed<T> {
  fn clone(& self) -> Self {
    HConsed { elm: self.elm.clone(), hkey: self.hkey }
  }
}

impl<T> PartialEq for HConsed<T> {
  #[inline(always)]
  fn eq(& self, rhs: & Self) -> bool {
    self.hkey == rhs.hkey
  }
}
impl<T> Eq for HConsed<T> {}
impl<T> PartialOrd for HConsed<T> {
  #[inline(always)]
  fn partial_cmp(& self, other: & Self) -> Option<Ordering> {
    self.hkey.partial_cmp(& other.hkey)
  }
}
impl<T> Ord for HConsed<T> {
  #[inline(always)]
  fn cmp(& self, other: & Self) -> Ordering {
    self.hkey.cmp(& other.hkey)
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

/** The consign storing the actual hash consed elements as `HConsed`s. */
pub struct HashConsign<T : Hash> {
  /** The actual hash consing table. */
  table: HashMap<u64, HConsed<T>>,
}

impl<T : Hash> HashConsign<T> {
  /** Creates an empty consign. */
  #[inline(always)]
  pub fn empty() -> Self {
    HashConsign {
      table: HashMap::new(),
    }
  }

  /** Creates an empty consign with a capacity. */
  #[inline(always)]
  pub fn empty_with_capacity(capacity: usize) -> Self {
    HashConsign {
      table: HashMap::with_capacity(capacity),
    }
  }

  /** The number of elements stored, mostly for testing. */
  #[inline(always)]
  pub fn len(& self) -> usize { self.table.len() }

  /** Hash conses something and returns the hash consed version. */
  pub fn mk(& mut self, elm: T) -> HConsed<T> {
    let mut hasher = SipHasher::new() ;
    let hkey = {
      elm.hash(& mut hasher) ;
      hasher.finish()
    } ;
    // If the element is known return it.
    if let Some(consed) = self.table.get(& hkey) { return consed.clone() } ;
    // Otherwise build the hash consed version...
    let consed = HConsed { elm: Arc::new(elm), hkey: hkey } ;
    // ...add it to the table...
    match self.table.insert(hkey, consed.clone()) {
      None => (), _ => unreachable!(),
    } ;
    // ...and return it.
    consed
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
