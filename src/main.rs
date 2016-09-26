// Copyright 2015 Adrien Champion. See the COPYRIGHT file at the top-level
// directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

extern crate hashconsing ;

mod unsync {
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
          t1 == t2,
        (& App(ref u1, ref v1), & App(ref u2, ref v2)) =>
          u1 == u2 && v1 == v2,
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

  pub fn run() {
    println!("\n|============================|\n|") ;

    println!("| Unsynced\n|") ;

    let mut consign = HashConsign::empty() ;
    assert_eq!(consign.len(), 0) ;

    let v = consign.var(0) ;
    println!("| [{:>2}] v = {}", consign.len(), v) ;
    assert_eq!(consign.len(), 1) ;

    let v2 = consign.var(3) ;
    println!("| [{:>2}] v2 = {}", consign.len(), v2) ;
    assert_eq!(consign.len(), 2) ;

    let lam = consign.lam(v2) ;
    println!("| [{:>2}] lam = {}", consign.len(), lam) ;
    assert_eq!(consign.len(), 3) ;

    let v3 = consign.var(3) ;
    println!("| [{:>2}] v3 = {}", consign.len(), v3) ;
    assert_eq!(consign.len(), 3) ;

    let lam2 = consign.lam(v3) ;
    println!("| [{:>2}] lam2 = {}", consign.len(), lam2) ;
    assert_eq!(consign.len(), 3) ;

    let app = consign.app(lam2, v) ;
    println!("| [{:>2}] app = {}", consign.len(), app) ;
    assert_eq!(consign.len(), 4) ;

    println!("|\n| {}", consign) ;

    println!("|\n|============================|\n") ;
  }

}

#[test]
fn unsynced() { unsync::run() }

#[cfg(not(test))]
fn main() {
  unsync::run() ;
}