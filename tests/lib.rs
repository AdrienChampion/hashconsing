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
fn test() {
  println!("\n|============================|\n|") ;

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

  println!("|\n|============================|\n") ;
}