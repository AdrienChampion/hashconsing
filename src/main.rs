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





mod sync {
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


  pub fn run(silent: bool) {
    println!("\n|============================|\n|") ;

    println!("| Synced\n|") ;

    let thread_count = 4 ;
    println!("| Running with {} threads\n|", thread_count) ;

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
        if i == 1 { ts.send(lam.clone()).unwrap() ; () } ;

        let v3 = consign.var(3) ;
        if ! silent {
          println!("| {} | [{:>2}] v3 = {}", i, consign.len(), v3)
        } ;
        if i == 2 { ts.send(v3.clone()).unwrap() ; () } ;

        let lam2 = consign.lam(v3) ;
        if ! silent {
          println!("| {} | [{:>2}] lam2 = {}", i, consign.len(), lam2)
        } ;
        if i == 3 { ts.send(lam2.clone()).unwrap() ; () } ;

        let app = consign.app(lam2, v) ;
        if ! silent {
          println!("| {} | [{:>2}] app = {}", i, consign.len(), app)
        } ;
        if i == 4 { ts.send(app).unwrap() ; () } ;

        tx.send(i)
      }) ;
    }

    for _ in 0..thread_count {
      match rs.recv() {
        Ok(t) => {
          bla = consign.app(t, bla) ;
          println!("| 0 | [{:>2}] app = {}", consign.len(), bla)
        },
        Err(e) => panic!("error {}.", e),
      }
      match rx.recv() {
        Ok(i) => println!("| Thread {} is done.", i),
        Err(e) => panic!("error {}.", e),
      }
    }
    assert_eq!(consign.len(), 7) ;

    println!("|\n| {}", * consign.lock().unwrap()) ;

    println!("|\n|============================|\n") ;
  }

}

#[test]
fn unsynced() { unsync::run() }
#[test]
fn synced() { sync::run(true) }

#[cfg(not(test))]
fn main() {
  unsync::run() ;
  sync::run(false)
}