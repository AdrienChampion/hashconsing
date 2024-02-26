//! Test/discussion on weak references to `HConsed` terms.
//!
//! Regular terms have type `HConsed<T>`, which are really references to `T` values in the factory.
//! `HConsed` terms can be turned into weak references which do not prevent the actual `T` values
//! from being dropped once there are no more *strong* (non-weak) reference pointing to them.
//!
//! This is typically useful when caching some form of result for your terms, for instance for term
//! evaluation.
//!
//! The code below defines a simple term structure, and a cache for term evaluation. The cache
//! stores weak references to the terms it caches the evaluation of, which does not block actual,
//! pointed-to (`T`) terms from being dropped as soon as possible.
//!
//! See the test at the bottom of this file for a demo.
#![allow(dead_code)]

use crate::*;

pub mod term {
    use super::*;

    pub type Term = HConsed<RawTerm>;

    #[derive(Hash, Clone, Copy, PartialEq, Eq)]
    pub enum Op {
        Add,
        Sub,
        Mul,
        Div,
    }
    impl Op {
        pub fn eval(self, lft: usize, rgt: usize) -> usize {
            match self {
                Self::Add => lft + rgt,
                Self::Sub => lft - rgt,
                Self::Mul => lft * rgt,
                Self::Div => lft / rgt,
            }
        }
    }

    #[derive(Hash, Clone, PartialEq, Eq)]
    pub enum RawTerm {
        Val(usize),
        App { op: Op, lft: Term, rgt: Term },
    }

    crate::consign! {
        /// Factory for terms.
        let FACTORY = consign(37) for RawTerm ;
    }

    pub fn factory_len() -> usize {
        FACTORY.try_read().expect("poisoned lock :/").len()
    }
    pub fn factory_clean() -> (usize, usize) {
        let pre = factory_len();
        FACTORY.collect();
        (pre, factory_len())
    }

    pub fn val(n: usize) -> Term {
        FACTORY.mk(RawTerm::Val(n))
    }
    pub fn app(op: Op, lft: Term, rgt: Term) -> Term {
        FACTORY.mk(RawTerm::App { op, lft, rgt })
    }
    pub fn add(lft: Term, rgt: Term) -> Term {
        app(Op::Add, lft, rgt)
    }
    pub fn sub(lft: Term, rgt: Term) -> Term {
        app(Op::Sub, lft, rgt)
    }
    pub fn mul(lft: Term, rgt: Term) -> Term {
        app(Op::Mul, lft, rgt)
    }
    pub fn div(lft: Term, rgt: Term) -> Term {
        app(Op::Div, lft, rgt)
    }

    pub struct Cache {
        cache: HashMap<WHConsed<RawTerm>, usize>,
    }
    impl Cache {
        pub fn new() -> Self {
            Self {
                cache: HashMap::with_capacity(101),
            }
        }
        /// True on cache hits.
        fn try_get(
            &mut self,
            term: &Term,
            compute: impl FnOnce(&mut Self) -> usize,
        ) -> (bool, usize) {
            if let Some(res) = self.cache.get(&term.to_weak()) {
                (true, *res)
            } else {
                let res = compute(self);
                let prev = self.cache.insert(term.to_weak(), res);
                debug_assert!(prev.is_none());
                (false, res)
            }
        }
    }

    fn eval_aux(term: &Term, cache: &mut Cache) -> usize {
        match *term.get() {
            RawTerm::Val(v) => v,
            RawTerm::App {
                op,
                ref lft,
                ref rgt,
            } => {
                let (_, lft) = eval(lft, cache);
                let (_, rgt) = eval(rgt, cache);
                op.eval(lft, rgt)
            }
        }
    }
    pub fn eval(term: &Term, cache: &mut Cache) -> (bool, usize) {
        cache.try_get(term, |cache| eval_aux(term, cache))
    }
}

fn show(blah: impl AsRef<str>, clean: bool) {
    println!("{}", blah.as_ref());
    if clean {
        let (pre, now) = term::factory_clean();
        debug_assert!(pre >= now);
        let rmed = pre - now;
        println!("- {} dead reference(s) removed", rmed);
        println!("  {} element(s) currently in the factory", now);
    } else {
        println!("- {} element(s) in the factory", term::factory_len());
    }
}

#[test]
fn run() {
    use term::{div, eval, factory_len, val, Cache};

    let cache = &mut Cache::new();

    println!("starting");
    show("starting", false);
    debug_assert_eq!(factory_len(), 0);

    {
        let seven = val(7);
        let two = val(2);
        show("created `7` and `2`", true);
        debug_assert_eq!(factory_len(), 2);

        let div = div(seven, two);
        show("created `7/2`", true);
        debug_assert_eq!(factory_len(), 3);

        let (cache_hit, res) = eval(&div, cache);
        println!("eval to {} (cache hit: {})", res, cache_hit);

        let (cache_hit, res) = eval(&div, cache);
        println!("eval to {} (cache hit: {})", res, cache_hit);

        show("about to drop all term references", true);
        debug_assert_eq!(factory_len(), 3);
    }

    println!();
    show("no term should be alive at this point", true);
    debug_assert_eq!(factory_len(), 0);
    // The factory was able to drop all (weak) refs to the terms we created, because our evaluation
    // cache stores **weak** refs to the terms it caches results for.
}
