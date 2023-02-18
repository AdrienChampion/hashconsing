//! Tests for the `collect` feature of the `HConsign`.

pub mod term {
    use crate::*;

    pub type Term = HConsed<RawTerm>;

    consign! {
        /// Factory for terms.
        let FACTORY = consign(37) for RawTerm ;
    }

    #[derive(Debug, Clone, Hash, PartialEq, Eq)]
    pub enum RawTerm {
        Cst(usize),
        Var(String),
        App { op: String, args: Vec<Term> },
    }

    pub fn cst(cst: usize) -> Term {
        FACTORY.mk(RawTerm::Cst(cst))
    }
    pub fn var(var: impl Into<String>) -> Term {
        FACTORY.mk(RawTerm::Var(var.into()))
    }
    pub fn app(op: impl Into<String>, args: Vec<Term>) -> Term {
        FACTORY.mk(RawTerm::App {
            op: op.into(),
            args,
        })
    }

    pub fn collect() {
        FACTORY.collect()
    }

    pub fn factory_do<T>(f: impl FnOnce(&mut HConsign<RawTerm>) -> T) -> T {
        f(&mut FACTORY.write().expect("factory is poisoned T_T"))
    }
}

use term::Term;

// (Sub)terms created by `create_1` and `create_2` do not overlap
pub fn create_1() -> Term {
    let var = term::var("v_2");
    let cst = term::cst(8);
    let app_1 = term::app("+", vec![var, cst]);
    term::app("-", vec![app_1])
}
pub fn create_2() -> Term {
    let var = term::var("v_1");
    let cst = term::cst(7);
    let app_1 = term::app("+", vec![var, cst]);
    term::app("-", vec![app_1])
}

pub fn factory_len() -> usize {
    term::factory_do(|f| f.len())
}

#[test]
pub fn collect() {
    println!("Factory is empty.");
    assert_eq!(factory_len(), 0);

    {
        let term = create_1();
        println!("Should have 4 terms in the factory right now.");
        assert_eq!(factory_len(), 4);
        assert_eq!(term.uid(), 3);
        println!("Collect should not do anything.");
        term::collect();
        assert_eq!(factory_len(), 4);
    };

    println!("Should still have 4 (weak) terms in the factory.");
    assert_eq!(factory_len(), 4);
    println!("Collect should drop everything in the table.");
    term::collect();
    assert_eq!(factory_len(), 0);

    {
        println!("Next uid should be `0`.");
        let leaf_t = term::var("v");
        assert_eq!(leaf_t.uid(), 0);
    }
    term::collect();
    assert_eq!(factory_len(), 0);

    let t = {
        println!("Putting 4 terms we're going to drop in the consign.");
        let _dont_care = create_1();
        println!("And then 4 other terms we keep.");
        create_2()
    };
    assert_eq!(factory_len(), 8);
    println!("Collect should drop the first 4 terms.");
    term::collect();
    assert_eq!(factory_len(), 4);

    println!("Now, the table's internal UID counter should not have changed.");
    assert_eq!(t.uid(), 7);
    println!("Next uid should be `8`.");
    let leaf_t = term::var("v");
    assert_eq!(leaf_t.uid(), 8);
}
