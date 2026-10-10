//! Tests for the arena-backed consign.

use std::{collections::HashSet, fmt};

use crate::{
    arena::{bumpalo::Bump, BHConsed, BHConsign},
    hash_coll::hashers::p_hash,
};

type Term<'b> = BHConsed<'b, ActualTerm<'b>>;

#[derive(Hash, PartialEq, Eq)]
enum ActualTerm<'b> {
    Var(usize),
    Lam(Term<'b>),
    App(Term<'b>, Term<'b>),
}

impl fmt::Display for ActualTerm<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Var(i) => write!(fmt, "v{i}"),
            Self::Lam(t) => write!(fmt, "({})", t.get()),
            Self::App(u, v) => write!(fmt, "{}.{}", u.get(), v.get()),
        }
    }
}

#[test]
fn run() {
    let arena = Bump::new();
    let mut factory: BHConsign<'_, ActualTerm<'_>> = BHConsign::with_capacity(&arena, 100);
    assert!(factory.is_empty());

    let (v1, is_new) = factory.mk_is_new(ActualTerm::Var(0));
    assert!(is_new);
    assert_eq!(factory.len(), 1);
    assert!(!factory.is_empty());

    let (v2, is_new) = factory.mk_is_new(ActualTerm::Var(3));
    assert!(is_new);
    assert_eq!(factory.len(), 2);

    let (lam, is_new) = factory.mk_is_new(ActualTerm::Lam(v2));
    assert!(is_new);
    assert_eq!(factory.len(), 3);

    let (v3, is_new) = factory.mk_is_new(ActualTerm::Var(3));
    assert!(!is_new);
    assert_eq!(factory.len(), 3);

    let (lam2, is_new) = factory.mk_is_new(ActualTerm::Lam(v3));
    assert!(!is_new);
    assert_eq!(factory.len(), 3);

    let (app, is_new) = factory.mk_is_new(ActualTerm::App(lam2, v1));
    assert!(is_new);
    assert_eq!(factory.len(), 4);

    assert_ne!(v1.uid(), v2.uid());
    assert_eq!(v2.uid(), v3.uid());
    assert_eq!(lam.uid(), lam2.uid());

    // Sharing is at the allocation, not just at the uid.
    assert!(std::ptr::eq(v2.get(), v3.get()));
    assert!(std::ptr::eq(lam.get(), lam2.get()));

    assert!(factory.contains(&ActualTerm::Var(3)));
    assert!(!factory.contains(&ActualTerm::Var(7)));

    assert_eq!(format!("{app}"), "(v3).v0");

    assert_eq!(factory.iter().count(), 4);
    assert_eq!(factory.consed_iter().count(), 4);
    for consed in factory.consed_iter() {
        assert!(factory.contains(consed.get()));
    }

    let mut set: HashSet<Term<'_>, p_hash::Builder> = HashSet::default();
    assert!(set.insert(v2));
    assert!(!set.insert(v3));
    for term in [v1, v2, lam, v3, lam2, app] {
        set.insert(term);
    }
    assert_eq!(set.len(), 4);
}

#[test]
fn copy_handles_need_no_clone() {
    fn takes<T>(_: T) {}

    let arena = Bump::new();
    let mut factory: BHConsign<'_, ActualTerm<'_>> = BHConsign::new(&arena);
    let v = factory.mk(ActualTerm::Var(0));
    takes(v);
    takes(v);
    assert_eq!(factory.len(), 1);
}
