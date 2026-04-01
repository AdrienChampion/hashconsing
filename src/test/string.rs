//! Some basic tests.

use std::fmt;

use crate::*;

type Term<S> = HConsed<ActualTerm<S>>;

type HTerm = HConsed<ActualTerm<String>>;

#[derive(Hash, Clone, PartialEq, Eq)]
enum ActualTerm<S>
{
    Var(S),
    Lam(Term<String>),
    App(Term<String>, Term<String>),
}

impl <S> fmt::Display for ActualTerm<S>
where S: Deref<Target=str> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Var(i) => write!(fmt, "v{}", i as &str),
            Self::Lam(t) => write!(fmt, "({})", t.get()),
            Self::App(u, v) => write!(fmt, "{}.{}", u.get(), v.get()),
        }
    }
}

trait TermFactory {
    fn var<S: Deref<Target = str>>(&mut self, v: S) -> HTerm;
    fn lam(&mut self, t: HTerm) -> HTerm;
    fn app(&mut self, u: HTerm, v: HTerm) -> HTerm;
}

impl TermFactory for HConsign<ActualTerm<String>> {
    fn var<S: Deref<Target = str>>(&mut self, v: S) -> HTerm {
        self.mk(ActualTerm::Var(v.to_string()))
    }
    fn lam(&mut self, t: HTerm) -> HTerm {
        self.mk(ActualTerm::Lam(t))
    }
    fn app(&mut self, u: HTerm, v: HTerm) -> HTerm{
        self.mk(ActualTerm::App(u, v))
    }
}

impl HashIntern<ActualTerm<String>> for ActualTerm<&str> {
    fn intern(&self) -> ActualTerm<String> {
        match self {
            ActualTerm::Var(v) => ActualTerm::Var(v.to_string()),
            ActualTerm::Lam(t) => ActualTerm::Lam(t.clone()),
            ActualTerm::App(u, v) => ActualTerm::App(u.clone(), v.clone()),
        }
    }
}

#[test]
#[allow(deprecated)]
fn run() {
    use coll::{HConMap, HConSet};

    let mut consign= HConsign::empty();
    assert_eq!(consign.len(), 0);

    let mut map: HConMap<Term<String>, _> = HConMap::with_capacity(100);
    let mut set: HConSet<Term<String>> = HConSet::with_capacity(100);

    let (v1, v1_name) = (consign.var("0"), "v1");
    println!("creating {v1}");
    assert_eq!(consign.len(), 1);
    let prev = map.insert(v1.clone(), v1_name);
    assert_eq!(prev, None);
    let is_new = set.insert(v1.clone());
    assert!(is_new);

    let (v2, v2_name) = (consign.var("3"), "v2");
    println!("creating {v2}");
    assert_eq!(consign.len(), 2);
    assert_ne!(v1.uid(), v2.uid());
    let prev = map.insert(v2.clone(), v2_name);
    assert_eq!(prev, None);
    let is_new = set.insert(v2.clone());
    assert!(is_new);

    let (lam, lam_name) = (consign.lam(v2.clone()), "lam");
    println!("creating {lam}");
    assert_eq!(consign.len(), 3);
    assert_ne!(v1.uid(), lam.uid());
    assert_ne!(v2.uid(), lam.uid());
    let prev = map.insert(lam.clone(), lam_name);
    assert_eq!(prev, None);
    let is_new = set.insert(lam.clone());
    assert!(is_new);

    let (v3, v3_name) = (consign.var("3"), "v3");
    println!("creating {v3}");
    assert_eq!(consign.len(), 3);
    assert_eq!(v2.uid(), v3.uid());
    let prev = map.insert(v3.clone(), v3_name);
    assert_eq!(prev, Some(v2_name));
    let is_new = set.insert(v3.clone());
    assert!(!is_new);

    let (lam2, lam2_name) = (consign.lam(v3), "lam2");
    println!("creating {lam2}");
    assert_eq!(consign.len(), 3);
    assert_eq!(lam.uid(), lam2.uid());
    let prev = map.insert(lam2.clone(), lam2_name);
    assert_eq!(prev, Some(lam_name));
    let is_new = set.insert(lam2.clone());
    assert!(!is_new);

    let (app, app_name) = (consign.app(lam2, v1), "app");
    println!("creating {app}");
    assert_eq!(consign.len(), 4);
    let prev = map.insert(app.clone(), app_name);
    assert_eq!(prev, None);
    let is_new = set.insert(app);
    assert!(is_new);

    for term in &set {
        assert!(map.contains_key(term))
    }
    for (term, val) in &map {
        println!("looking for `{val}`");
        assert!(set.contains(term))
    }
}

