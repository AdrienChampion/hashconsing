#![doc="
Hash consing library. Ideally we'd like to use weak references in the consign
but they're currently feature gated. Waiting for dust to settle.

See the [paper by Filiâtre and Conchon](http://dl.acm.org/citation.cfm?doid=1159876.1159880).

# Example

Simple example for lambda calculus.

```
#[macro_use]
extern crate hashconsing as hc ;

use std::fmt ;
use ::hc::* ;


use ::ActualTerm::* ;

hash_cons!{pub Term for ActualTerm}


pub enum ActualTerm {
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
        t1.uid() == t2.uid(),
      (& App(ref u1, ref v1), & App(ref u2, ref v2)) =>
        u1.uid() == u2.uid() && v1.uid() == v2.uid(),
      _ => false
    }
  }
}
impl Eq for ActualTerm {}
impl UID for ActualTerm {
  fn uid(& self) -> usize {
    match self {
      & Var(i) => i,
      & Lam(ref t) => 19 * t.uid() + 1,
      & App(ref u, ref v) => 19 * (19 * u.uid() + v.uid()) + 2,
    }
  }
}
impl Hash for ActualTerm {
  fn hash<H>(& self, state: & mut H) where H: Hasher {
    self.uid().hash(state)
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
```
"]

use std::fmt ;
use std::rc::{ Rc } ;
use std::collections::HashMap ;
pub use std::hash::{ Hash, Hasher } ;
use std::cmp::{
  PartialEq, Eq, PartialOrd, Ord, Ordering
} ;

/**
Creates a hash consed type for some type.

```
hash_cons!{pub Term for ActualTerm}

pub enum ActualTerm {
  Var(usize),
  Lam(Term),
  App(Term, Term)
}
```
**/
#[macro_export]
macro_rules! hash_cons {
  ($id:ident for $t:ty) => (
    type $id = ::std::rc::Rc<$crate::HashConsed<$t>> ;
  ) ;
  (pub $id:ident for $t:ty) => (
    pub type $id = ::std::rc::Rc<$crate::HashConsed<$t>> ;
  ) ;
}

/// Can produce a **unique** identifier. Used for hashing.
pub trait UID: Eq {
  /// Returns a unique identifier.
  fn uid(& self) -> usize ;
}


/**
Stores a hash consed element and its hash in order to avoid recomputing it
every time. The type stored by the consign is actually
```Rc<HashConsed<T>>```.
**/
pub struct HashConsed<T> where T: UID {
  /// The actual element.
  elm: T,
  /// Stores the hash key of the element.
  hkey: usize,
}

impl<T: UID> HashConsed<T> {
  /// The element hash consed.
  pub fn get(& self) -> & T { & self.elm }
}

impl<T: UID> UID for HashConsed<T> {
  fn uid(& self) -> usize { self.hkey }
}

impl<T: UID> PartialEq for HashConsed<T> {
  fn eq(& self, rhs: & Self) -> bool {
    self.hkey == rhs.hkey
  }
}
impl<T: UID> Eq for HashConsed<T> {}
impl<T: UID> PartialOrd for HashConsed<T> {
  fn partial_cmp(& self, other: & Self) -> Option<Ordering> {
    self.hkey.partial_cmp(& other.hkey)
  }
}
impl<T: UID> Ord for HashConsed<T> {
  fn cmp(& self, other: & Self) -> Ordering {
    self.hkey.cmp(& other.hkey)
  }
}
impl<T: UID> Hash for HashConsed<T> {
  fn hash<H>(& self, state: & mut H) where H: Hasher {
    self.hkey.hash(state)
  }
}

impl<T: UID + fmt::Display> fmt::Display for HashConsed<T> {
  fn fmt(& self, fmt: & mut fmt::Formatter) -> fmt::Result {
    write!(fmt, "{}", self.elm)
  }
}



/// The consign storing the actual hash consed elements as `Rc`s.
pub struct HashConsign<T> where T: UID + Hash {
  /// The actual hash consing table.
  table: HashMap<usize, Rc<HashConsed<T>>>,
}

impl<T> HashConsign<T> where T: UID + Hash {
  /// Creates an empty consign.
  pub fn empty() -> Self {
    HashConsign { table: HashMap::new() }
  }

  /// Creates an empty consign with a capacity.
  pub fn empty_with_capacity(capacity: usize) -> Self {
    HashConsign { table: HashMap::with_capacity(capacity) }
  }

  /// Hash conses something and returns the hash consed version.
  pub fn mk(& mut self, elm: T) -> Rc<HashConsed<T>>
  where HashConsed<T>: Sized {
    let hkey = elm.uid() ;
    let _ = match self.table.get(& hkey) {
      Some(consed) => return consed.clone(),
      _ => (),
    } ;
    let consed = Rc::new(HashConsed{ elm: elm, hkey: hkey }) ;
    match self.table.insert(hkey, consed.clone()) {
      None => (),
      _ => unreachable!(),
    } ;
    consed
  }

  /// The number of elements stored, mostly for testing.
  pub fn len(& self) -> usize { self.table.len() }
}


// Code for weak refs in consign

// /// The consign storing the actual hash consed elements as `Rc`s.
// pub struct HashConsign<T> where T: UID + Hash {
//   table: HashMap<usize, Weak<HashConsed<T>>>,
// }

// impl<T> HashConsign<T> where T: UID + Hash {
// /// Creates an empty consign.
// pub fn empty() -> Self {
//   HashConsign { table: HashMap::new() }
// }

// /// Creates an empty consign with a capacity.
// pub fn empty_with_capacity(capacity: usize) -> Self {
//   HashConsign { table: HashMap::new_with_capacity(capacity) }
// }

// /// Hash conses something and returns the hash consed version.
// pub fn empty() -> Self {
//   HashConsign { table: HashMap::new() }
// }
// fn mk(& mut self, elm: T) -> Rc<HashConsed<T>>
// where HashConsed<T>: Sized {
//   let hkey = elm.uid() ;
//   let should_remove = match self.table.get(& hkey) {
//     Some(consed) => match consed.upgrade() {
//       Some(consed) => return consed,
//       _ => true,
//     },
//     _ => false,
//   } ;
//   if should_remove { self.table.remove(& hkey) ; () } ;
//   let consed = Rc::new(HashConsed{ elm: elm, hkey: hkey }) ;
//   let wconsed = consed.downgrade() ;
//   match self.table.insert(hkey, wconsed) {
//     None => (),
//     _ => panic!("unreachable"),
//   } ;
//   consed
// }

//   /// The number of elements stored, mostly for testing.
//   pub fn len(& self) -> usize { self.table.len() }
// }
