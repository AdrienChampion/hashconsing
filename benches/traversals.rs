#![feature(test)]
//! Test the performance of set-backed traversals

use std::fmt;

use hashconsing::*;

type Term = HConsed<ActualTerm>;

const TERM_SIZE: usize = 10_000;

consign! {
    /// Factory for terms.
    let TERMS = consign(TERM_SIZE) for ActualTerm ;
}

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
enum Op {
    Var(usize),
    Lam,
    App,
}

impl Op {
    fn arity(&self) -> usize {
        match self {
            Op::Var(_) => 0,
            Op::Lam => 1,
            Op::App => 2,
        }
    }
}

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
struct ActualTerm {
    op: Op,
    children: Vec<Term>,
}

impl fmt::Display for ActualTerm {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match &self.op {
            &Op::Var(i) => write!(fmt, "v{}", i),
            &Op::Lam => write!(fmt, "({})", self.children[0]),
            &Op::App => write!(fmt, "{}.{}", self.children[0], self.children[1]),
        }
    }
}

impl ActualTerm {
    fn new(op: Op, children: Vec<Term>) -> ActualTerm {
        ActualTerm {
            op,
            children,
        }
    }
}

/// A distribution of n usizes that sum to this value.
/// (n, sum)
pub struct Sum(usize, usize);
impl rand::distributions::Distribution<Vec<usize>> for Sum {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> Vec<usize> {
        use rand::seq::SliceRandom;
        let mut acc = self.1;
        let mut ns = Vec::new();
        assert!(acc == 0 || self.0 > 0);
        while acc > 0 && ns.len() < self.0 {
            let x = rng.gen_range(0..acc);
            acc -= x;
            ns.push(x);
        }
        while ns.len() < self.0 {
            ns.push(0);
        }
        if acc > 0 {
            *ns.last_mut().unwrap() += acc;
        }
        ns.shuffle(rng);
        ns
    }
}

pub struct TermDist {
    subterm_count: usize,
    binding_depth: usize,
}

impl TermDist {
    /// Generates a function of one argument of the given size
    fn fn_of_size(subterm_count: usize) -> Self {
        Self {
            subterm_count,
            binding_depth: 1,
        }
    }
    fn new(subterm_count: usize, binding_depth: usize) -> Self {
        Self {
            subterm_count,
            binding_depth,
        }
    }
}

impl rand::distributions::Distribution<Term> for TermDist {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> Term {
        use rand::seq::SliceRandom;
        let ops = &[
            Op::Var(rng.gen_range(0..self.binding_depth)),
            Op::Lam,
            Op::App,
        ];
        let o = match self.subterm_count {
            1 => ops[..1].choose(rng),  // arity 0
            2 => ops[1..2].choose(rng), // arity 1
            _ => ops[1..].choose(rng),  // others
        }
        .unwrap()
        .clone();
        // Now, self.0 is a least arity+1
        let excess = self.subterm_count - 1 - o.arity();
        let ns = Sum(o.arity(), excess).sample(rng);
        let subterms = ns
            .into_iter()
            .map(|n| TermDist::new(n + 1, if o == Op::Lam { self.binding_depth + 1 } else { self.binding_depth }).sample(rng))
            .collect::<Vec<_>>();
        TERMS.mk(ActualTerm::new(o, subterms))
    }
}

/// Iterator over subterms, children-first
struct PostOrderIter {
    // [(childred addded, term)]
    stack: Vec<(bool, Term)>,
    visited: coll::HConSet<Term>,
}

impl PostOrderIter {
    fn new(root: Term) -> Self {
        Self {
            stack: vec![(false, root)],
            visited: coll::HConSet::<Term>::new(),
        }
    }
}

impl std::iter::Iterator for PostOrderIter {
    type Item = Term;
    fn next(&mut self) -> Option<Term> {
        while let Some((children_pushed, t)) = self.stack.last() {
            if self.visited.contains(&t) {
                self.stack.pop();
            } else if !children_pushed {
                self.stack.last_mut().unwrap().0 = true;
                let last = self.stack.last().unwrap().1.clone();
                self.stack
                    .extend(last.children.iter().map(|c| (false, c.clone())));
            } else {
                break;
            }
        }
        self.stack.pop().map(|(_, t)| {
            self.visited.insert(t.clone());
            t
        })
    }
}
extern crate test;

#[cfg(test)]
mod tests {
    use test::{Bencher, black_box};
    use super::*;
    use rand::distributions::Distribution;

    #[bench]
    fn bench_traversal(b: &mut Bencher) {
        let rng = &mut rand::thread_rng();
        let t = TermDist::fn_of_size(TERM_SIZE).sample(rng);
        b.iter(|| {
            black_box(PostOrderIter::new(t.clone()).count())
        });
    }
}
