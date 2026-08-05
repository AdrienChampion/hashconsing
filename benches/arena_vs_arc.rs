//! Compares the `Arc`-backed (`HConsed`) and arena-backed (`BHConsed`) consigns on the same
//! generated lambda-calculus term DAG.
//!
//! Run with: `cargo bench --features bumpalo --bench arena_vs_arc`

use std::{
    collections::HashSet,
    hint::black_box,
    time::{Duration, Instant},
};

use hashconsing::{
    arena::{bumpalo::Bump, BHConsed, BHConsign},
    hash_coll::{hashers::p_hash::Builder as PHash, p_hash::HConSet},
    HConsed, HConsign, HashConsign,
};

/// Instructions per run.
const N: usize = 200_000;
/// Repetitions; the minimum over repetitions is reported.
const REPS: usize = 5;
/// Distinct variables the generator draws from.
const VARS: usize = 16;
const SEED: u64 = 0x5EED_1234_ABCD_9876;

type ATerm = HConsed<ArcTerm>;

#[derive(Hash, Clone, PartialEq, Eq)]
enum ArcTerm {
    Var(usize),
    Lam(ATerm),
    App(ATerm, ATerm),
}

type BTerm<'b> = BHConsed<'b, BumpTerm<'b>>;

#[derive(Hash, PartialEq, Eq)]
enum BumpTerm<'b> {
    Var(usize),
    Lam(BTerm<'b>),
    App(BTerm<'b>, BTerm<'b>),
}

#[derive(Clone, Copy)]
enum Instr {
    Var(usize),
    Lam(usize),
    App(usize, usize),
}

struct Rng(u64);
impl Rng {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    }
    fn below(&mut self, n: usize) -> usize {
        (self.next() % n as u64) as usize
    }
}

/// Generates the instruction sequence replayed against both consigns.
///
/// Every operand index is `< i`, so replay never refers to an unbuilt term.
fn program(n: usize) -> Vec<Instr> {
    let mut rng = Rng(SEED);
    let mut prog = Vec::with_capacity(n);
    for i in 0..n {
        prog.push(if i < VARS {
            Instr::Var(i)
        } else {
            match rng.below(3) {
                0 => Instr::Var(rng.below(VARS)),
                1 => Instr::Lam(rng.below(i)),
                _ => Instr::App(rng.below(i), rng.below(i)),
            }
        });
    }
    prog
}

fn build_arc(prog: &[Instr]) -> (HConsign<ArcTerm>, Vec<ATerm>) {
    let mut factory = HConsign::with_capacity(prog.len());
    let mut terms: Vec<ATerm> = Vec::with_capacity(prog.len());
    for instr in prog {
        let t = match *instr {
            Instr::Var(i) => factory.mk(ArcTerm::Var(i)),
            Instr::Lam(a) => factory.mk(ArcTerm::Lam(terms[a].clone())),
            Instr::App(a, b) => factory.mk(ArcTerm::App(terms[a].clone(), terms[b].clone())),
        };
        terms.push(t);
    }
    (factory, terms)
}

fn reintern_arc(prog: &[Instr], factory: &mut HConsign<ArcTerm>, terms: &[ATerm]) {
    for instr in prog {
        let t = match *instr {
            Instr::Var(i) => factory.mk(ArcTerm::Var(i)),
            Instr::Lam(a) => factory.mk(ArcTerm::Lam(terms[a].clone())),
            Instr::App(a, b) => factory.mk(ArcTerm::App(terms[a].clone(), terms[b].clone())),
        };
        black_box(&t);
    }
}

fn traverse_arc(terms: &[ATerm]) -> usize {
    let mut visited: HConSet<ATerm> = HConSet::with_capacity(terms.len());
    let mut stack: Vec<ATerm> = Vec::with_capacity(64);
    for root in terms {
        stack.push(root.clone());
        while let Some(t) = stack.pop() {
            if !visited.insert(t.clone()) {
                continue;
            }
            match t.get() {
                ArcTerm::Var(_) => (),
                ArcTerm::Lam(b) => stack.push(b.clone()),
                ArcTerm::App(f, a) => {
                    stack.push(f.clone());
                    stack.push(a.clone());
                }
            }
        }
    }
    visited.len()
}

fn build_arena<'b>(
    arena: &'b Bump,
    prog: &[Instr],
) -> (BHConsign<'b, BumpTerm<'b>>, Vec<BTerm<'b>>) {
    let mut factory = BHConsign::with_capacity(arena, prog.len());
    let mut terms = Vec::with_capacity(prog.len());
    for instr in prog {
        let t = match *instr {
            Instr::Var(i) => factory.mk(BumpTerm::Var(i)),
            Instr::Lam(a) => factory.mk(BumpTerm::Lam(terms[a])),
            Instr::App(a, b) => factory.mk(BumpTerm::App(terms[a], terms[b])),
        };
        terms.push(t);
    }
    (factory, terms)
}

fn reintern_arena<'b>(
    prog: &[Instr],
    factory: &mut BHConsign<'b, BumpTerm<'b>>,
    terms: &[BTerm<'b>],
) {
    for instr in prog {
        let t = match *instr {
            Instr::Var(i) => factory.mk(BumpTerm::Var(i)),
            Instr::Lam(a) => factory.mk(BumpTerm::Lam(terms[a])),
            Instr::App(a, b) => factory.mk(BumpTerm::App(terms[a], terms[b])),
        };
        black_box(&t);
    }
}

fn traverse_arena(terms: &[BTerm<'_>]) -> usize {
    let mut visited: HashSet<BTerm<'_>, PHash> =
        HashSet::with_capacity_and_hasher(terms.len(), PHash::new());
    let mut stack: Vec<BTerm<'_>> = Vec::with_capacity(64);
    for root in terms {
        stack.push(*root);
        while let Some(t) = stack.pop() {
            if !visited.insert(t) {
                continue;
            }
            match t.get() {
                BumpTerm::Var(_) => (),
                BumpTerm::Lam(b) => stack.push(*b),
                BumpTerm::App(f, a) => {
                    stack.push(*f);
                    stack.push(*a);
                }
            }
        }
    }
    visited.len()
}

/// Timings for `build`, `re-intern`, `traverse` and `teardown`.
fn run_arc(prog: &[Instr]) -> [Duration; 4] {
    let t = Instant::now();
    let (mut factory, terms) = build_arc(prog);
    let build = t.elapsed();

    let t = Instant::now();
    reintern_arc(prog, &mut factory, &terms);
    let reintern = t.elapsed();

    let t = Instant::now();
    black_box(traverse_arc(&terms));
    let traverse = t.elapsed();

    let t = Instant::now();
    drop(terms);
    drop(factory);
    let teardown = t.elapsed();

    [build, reintern, traverse, teardown]
}

/// Same phases as [`run_arc`]; arena creation is charged to `build`, arena release to `teardown`.
fn run_arena(prog: &[Instr]) -> [Duration; 4] {
    let t = Instant::now();
    let arena = Bump::new();
    let (mut factory, terms) = build_arena(&arena, prog);
    let build = t.elapsed();

    let t = Instant::now();
    reintern_arena(prog, &mut factory, &terms);
    let reintern = t.elapsed();

    let t = Instant::now();
    black_box(traverse_arena(&terms));
    let traverse = t.elapsed();

    let t = Instant::now();
    drop(terms);
    drop(factory);
    drop(arena);
    let teardown = t.elapsed();

    [build, reintern, traverse, teardown]
}

fn main() {
    let prog = program(N);
    let (mut arc, mut arena) = ([Duration::MAX; 4], [Duration::MAX; 4]);

    for _ in 0..REPS {
        let (a, b) = (run_arc(&prog), run_arena(&prog));
        for i in 0..4 {
            arc[i] = arc[i].min(a[i]);
            arena[i] = arena[i].min(b[i]);
        }
    }

    let ms = |d: Duration| d.as_secs_f64() * 1e3;
    let speedup = |a: Duration, b: Duration| a.as_secs_f64() / b.as_secs_f64();
    let names = ["build", "re-intern", "traverse", "teardown"];

    println!("{N} instructions, min of {REPS} reps\n");
    println!(
        "{:<12}{:>14}{:>18}{:>10}",
        "phase", "Arc (HConsed)", "Arena (BHConsed)", "speedup"
    );
    for i in 0..4 {
        println!(
            "{:<12}{:>11.3} ms{:>15.3} ms{:>9.2}x",
            names[i],
            ms(arc[i]),
            ms(arena[i]),
            speedup(arc[i], arena[i])
        );
    }
    let (ta, tb): (Duration, Duration) = (arc.iter().sum(), arena.iter().sum());
    println!(
        "{:<12}{:>11.3} ms{:>15.3} ms{:>9.2}x",
        "total",
        ms(ta),
        ms(tb),
        speedup(ta, tb)
    );

    assert!(
        speedup(ta, tb) >= 2.0,
        "total speedup {:.2}x < 2.0x",
        speedup(ta, tb)
    );
    assert!(
        speedup(arc[2], arena[2]) >= 1.5,
        "traverse speedup {:.2}x < 1.5x",
        speedup(arc[2], arena[2])
    );
    assert!(
        speedup(arc[3], arena[3]) >= 3.0,
        "teardown speedup {:.2}x < 3.0x",
        speedup(arc[3], arena[3])
    );
}
