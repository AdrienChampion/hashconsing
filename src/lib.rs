#![doc="
Hash consing library. Ideally we'd like to use weak references in the consign
but they're currently feature gated. Waiting for dust to settle.

Module `sync` contains a thread-safe version.

See the [paper by Filiâtre and Conchon](http://dl.acm.org/citation.cfm?doid=1159876.1159880).

# Example

Simple example for lambda calculus.

```
#[macro_use]
extern crate hashconsing as hc ;

use std::rc::Rc ;
use std::fmt ;
use hc::* ;


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


fn main() {
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
*/
#[macro_export]
macro_rules! hash_cons {
  ($id:ident for $t:ty) => (
    type $id = ::std::rc::Rc<$crate::HashConsed<$t>> ;
  ) ;
  (pub $id:ident for $t:ty) => (
    pub type $id = ::std::rc::Rc<$crate::HashConsed<$t>> ;
  ) ;
}

/**
Creates the `mk` function on a consign used to hash cons elements. Used to
factor the synced and unsynced versions.
*/
macro_rules! consign_mk_fun {
  (synced for $t:ty) => (
    pub fn mk(& self, elm: $t) -> HConsed<$t> {
      consign_mk_fun!( self.table.lock().unwrap(), elm, Arc )
    }
  ) ;
  (unsynced for $t:ty) => (
    pub fn mk(& mut self, elm: $t) -> HConsed<$t> {
      consign_mk_fun!( self.table, elm, Rc )
    }
  ) ;
  ($tbl_expr:expr, $elm:ident, $ref_type:ident) => ({
    let table = & mut $tbl_expr ;
    let hkey = $elm.uid() ;
    // If the element is known return it.
    if let Some(consed) = table.get(& hkey) { return consed.clone() } ;
    // Otherwise build the hash consed version...
    let consed = $ref_type::new(HashConsed{ elm: $elm, hkey: hkey }) ;
    // ...add it to the table...
    match table.insert(hkey, consed.clone()) {
      None => (), _ => unreachable!(),
    } ;
    // ...and return it.
    consed
  }) ;
}



/// Can produce a **unique** identifier. Used for hashing.
pub trait UID: Eq {
  /// Returns a unique identifier.
  fn uid(& self) -> usize ;
}


/**
Stores a hash consed element and its hash in order to avoid recomputing it
every time. A (synced) consign stores stores `Rc`s (`Arc`s) of that type for
(thread-safe) sharing.
*/
pub struct HashConsed<T> {
  /// The actual element.
  elm: T,
  /// Stores the hash key of the element.
  hkey: usize,
}

impl<T> HashConsed<T> {
  /// The element hash consed.
  pub fn get(& self) -> & T { & self.elm }
}

impl<T> UID for HashConsed<T> {
  fn uid(& self) -> usize { self.hkey }
}

impl<T> PartialEq for HashConsed<T> {
  fn eq(& self, rhs: & Self) -> bool {
    self.hkey == rhs.hkey
  }
}
impl<T> Eq for HashConsed<T> {}
impl<T> PartialOrd for HashConsed<T> {
  fn partial_cmp(& self, other: & Self) -> Option<Ordering> {
    self.hkey.partial_cmp(& other.hkey)
  }
}
impl<T> Ord for HashConsed<T> {
  fn cmp(& self, other: & Self) -> Ordering {
    self.hkey.cmp(& other.hkey)
  }
}
impl<T> Hash for HashConsed<T> {
  fn hash<H>(& self, state: & mut H) where H: Hasher {
    self.hkey.hash(state)
  }
}

impl<T: fmt::Display> fmt::Display for HashConsed<T> {
  fn fmt(& self, fmt: & mut fmt::Formatter) -> fmt::Result {
    write!(fmt, "{}", self.elm)
  }
}

/// Actual type stored and returned by the consign.
pub type HConsed<T> = Rc<HashConsed<T>> ;

/// The consign storing the actual hash consed elements as `Rc`s.
pub struct HashConsign<T> where T: UID + Hash {
  /// The actual hash consing table.
  table: HashMap<usize, HConsed<T>>,
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
  consign_mk_fun!{ unsynced for T }

  /// The number of elements stored, mostly for testing.
  pub fn len(& self) -> usize { self.table.len() }
}

impl<T> fmt::Display for HashConsign<T> where T: UID + Hash + fmt::Display {
  fn fmt(& self, fmt: & mut fmt::Formatter) -> fmt::Result {
    try!( write!(fmt, "consign:") ) ;
    for (_, ref e) in self.table.iter() {
      try!( write!(fmt, "\n  | {}", e) ) ;
    }
    Ok(())
  }
}



/**
Thread safe version of the hash consed library.

# Example

Lamda calculus with channel based communication.

```
#[macro_use]
extern crate hashconsing ;

use std::thread ;
use std::sync::{ Arc, mpsc } ;
use std::fmt ;

use hashconsing::sync::* ;

use self::ActualTerm::* ;

sync_hash_cons!{pub Term for ActualTerm}


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
}


impl TermFactory for HashConsign<ActualTerm> {
  fn var(& self, v: usize) -> Term { self.mk( Var(v) ) }
  fn lam(& self, t: Term) -> Term { self.mk( Lam(t) ) }
  fn app(& self, u: Term, v: Term) -> Term {
    self.mk( App(u, v) )
  }
}


pub fn main() {
  let silent = false ;
  println!("\n|============================|\n|") ;

  println!("| Unsynced\n|") ;

  let thread_count = 4 ;
  println!("| Running with {} threads\n|", thread_count) ;

  let consign = Arc::new(HashConsign::empty()) ;
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
      if ! silent {
        println!("| {} | [{:>2}] v = {}", i, consign.len(), v)
      } ;

      let v2 = consign.var(3) ;
      if ! silent {
        println!("| {} | [{:>2}] v2 = {}", i, consign.len(), v2)
      } ;

      let lam = consign.lam(v2) ;
      if ! silent {
        println!("| {} | [{:>2}] lam = {}", i, consign.len(), lam)
      } ;
      if i == 1 { consign.send(& lam, & ts).unwrap() ; () } ;

      let v3 = consign.var(3) ;
      if ! silent {
        println!("| {} | [{:>2}] v3 = {}", i, consign.len(), v3)
      } ;
      if i == 2 { consign.send(& v3, & ts).unwrap() ; () } ;

      let lam2 = consign.lam(v3) ;
      if ! silent {
        println!("| {} | [{:>2}] lam2 = {}", i, consign.len(), lam2)
      } ;
      if i == 3 { consign.send(& lam2, & ts).unwrap() ; () } ;

      let app = consign.app(lam2, v) ;
      if ! silent {
        println!("| {} | [{:>2}] app = {}", i, consign.len(), app)
      } ;
      if i == 4 { consign.send(& app, & ts).unwrap() ; () } ;

      tx.send(i)
    }) ;
  }

  for _ in 0..thread_count {
    match rs.recv() {
      Ok(t) => {
        bla = consign.app(t, bla) ;
        println!("| * | [{:>2}] app = {}", consign.len(), bla)
      },
      Err(e) => println!("| Error {}.", e),
    }
    match rx.recv() {
      Ok(i) => println!("| Thread {} is done.", i),
      Err(e) => println!("| Error {}.", e),
    }
  }
  assert_eq!(consign.len(), 7) ;

  println!("|\n| {}", consign) ;

  println!("|\n|============================|\n")
}
```
*/
pub mod sync {
  use std::fmt ;
  use std::collections::HashMap ;
  use std::sync::{ Arc, Mutex } ;  
  use std::sync::mpsc::{ Sender, SendError } ;
  pub use ::{ Hash, Hasher } ;
  pub use ::{ UID, HashConsed } ;

  /**
  Creates a thread safe hash consed type for some type.
  */
  #[macro_export]
  macro_rules! sync_hash_cons {
    ($id:ident for $t:ty) => (
      type $id = ::std::sync::Arc<$crate::HashConsed<$t>> ;
    ) ;
    (pub $id:ident for $t:ty) => (
      pub type $id = ::std::sync::Arc<$crate::HashConsed<$t>> ;
    ) ;
  }


  unsafe impl<T> Sync for HashConsed<T> { }

  /// Actual type stored and returnd by the sync consigned.
  pub type HConsed<T> = Arc<HashConsed<T>> ;


  /**
  The consign storing the actual hash consed elements as `Rc`s. The hash map is
  wrapped in a mutex for thread-safety.
  */
  pub struct HashConsign<T> where T: UID + Hash {
    /// The actual hash consing table.
    table: Mutex<HashMap<usize, HConsed<T>>>,
  }

  impl<T> HashConsign<T> where T: UID + Hash {
    /// Creates an empty consign.
    pub fn empty() -> Self {
      HashConsign { table: Mutex::new(HashMap::new()) }
    }

    /// Creates an empty consign with a capacity.
    pub fn empty_with_capacity(capacity: usize) -> Self {
      HashConsign { table: Mutex::new(HashMap::with_capacity(capacity)) }
    }

    /// Hash conses something and returns the hash consed version.
    consign_mk_fun!{ synced for T }

    /// Sends a hash consed element through a send channel.
    pub fn send(
      & self, elm: & HConsed<T>, sender: & Sender<HConsed<T>>
    ) -> Result<(), SendError<HConsed<T>>> {
      sender.send(elm.clone())
    }

    /// The number of elements stored, mostly for testing.
    pub fn len(& self) -> usize { self.table.lock().unwrap().len() }
  }

  unsafe impl<T> Sync for HashConsign<T> where T: UID + Hash { }

  impl<T> fmt::Display for HashConsign<T> where T: UID + Hash + fmt::Display {
    fn fmt(& self, fmt: & mut fmt::Formatter) -> fmt::Result {
      try!( write!(fmt, "consign:") ) ;
      let table = self.table.lock().unwrap() ;
      for (_, ref e) in table.iter() {
        try!( write!(fmt, "\n  | {}", e) ) ;
      }
      Ok(())
    }
  }



}