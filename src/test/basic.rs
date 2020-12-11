//! Some basic tests.

use std::fmt;

use crate::*;

type Term = HConsed<ActualTerm>;

#[derive(Hash, Clone, PartialEq, Eq)]
enum ActualTerm {
    Var(usize),
    Lam(Term),
    App(Term, Term),
}

impl fmt::Display for ActualTerm {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Self::Var(i) => write!(fmt, "v{}", i),
            &Self::Lam(ref t) => write!(fmt, "({})", t.get()),
            &Self::App(ref u, ref v) => write!(fmt, "{}.{}", u.get(), v.get()),
        }
    }
}

trait TermFactory {
    fn var(&mut self, v: usize) -> Term;
    fn lam(&mut self, t: Term) -> Term;
    fn app(&mut self, u: Term, v: Term) -> Term;
}

impl TermFactory for HConsign<ActualTerm> {
    fn var(&mut self, v: usize) -> Term {
        self.mk(ActualTerm::Var(v))
    }
    fn lam(&mut self, t: Term) -> Term {
        self.mk(ActualTerm::Lam(t))
    }
    fn app(&mut self, u: Term, v: Term) -> Term {
        self.mk(ActualTerm::App(u, v))
    }
}

#[test]
fn run() {
    use coll::{HConMap, HConSet};

    let mut consign = HConsign::empty();
    assert_eq!(consign.len(), 0);

    let mut map: HConMap<Term, _> = HConMap::with_capacity(100);
    let mut set: HConSet<Term> = HConSet::with_capacity(100);

    let (v1, v1_name) = (consign.var(0), "v1");
    println!("creating {}", v1);
    assert_eq!(consign.len(), 1);
    let prev = map.insert(v1.clone(), v1_name);
    assert_eq!(prev, None);
    let is_new = set.insert(v1.clone());
    assert!(is_new);

    let (v2, v2_name) = (consign.var(3), "v2");
    println!("creating {}", v2);
    assert_eq!(consign.len(), 2);
    assert_ne!(v1.uid(), v2.uid());
    let prev = map.insert(v2.clone(), v2_name);
    assert_eq!(prev, None);
    let is_new = set.insert(v2.clone());
    assert!(is_new);

    let (lam, lam_name) = (consign.lam(v2.clone()), "lam");
    println!("creating {}", lam);
    assert_eq!(consign.len(), 3);
    assert_ne!(v1.uid(), lam.uid());
    assert_ne!(v2.uid(), lam.uid());
    let prev = map.insert(lam.clone(), lam_name);
    assert_eq!(prev, None);
    let is_new = set.insert(lam.clone());
    assert!(is_new);

    let (v3, v3_name) = (consign.var(3), "v3");
    println!("creating {}", v3);
    assert_eq!(consign.len(), 3);
    assert_eq!(v2.uid(), v3.uid());
    let prev = map.insert(v3.clone(), v3_name);
    assert_eq!(prev, Some(v2_name));
    let is_new = set.insert(v3.clone());
    assert!(!is_new);

    let (lam2, lam2_name) = (consign.lam(v3.clone()), "lam2");
    println!("creating {}", lam2);
    assert_eq!(consign.len(), 3);
    assert_eq!(lam.uid(), lam2.uid());
    let prev = map.insert(lam2.clone(), lam2_name);
    assert_eq!(prev, Some(lam_name));
    let is_new = set.insert(lam2.clone());
    assert!(!is_new);

    let (app, app_name) = (consign.app(lam2, v1), "app");
    println!("creating {}", app);
    assert_eq!(consign.len(), 4);
    let prev = map.insert(app.clone(), app_name);
    assert_eq!(prev, None);
    let is_new = set.insert(app.clone());
    assert!(is_new);

    for term in &set {
        assert!(map.contains_key(term))
    }
    for (term, val) in &map {
        println!("looking for `{}`", val);
        assert!(set.contains(term))
    }
}
