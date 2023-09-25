use hashconsing::*;
use rand::distributions::Distribution;

type Term = HConsed<ActualTerm>;

const TERM_SIZE: usize = 10_000;

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
enum Op
{
	Var(usize),
	Lam,
	App,
}

impl Op
{
	fn arity(&self) -> usize
	{
		match self {
			Op::Var(_) => 0,
			Op::Lam => 1,
			Op::App => 2,
		}
	}
}

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
struct ActualTerm
{
	op: Op,
	children: Vec<Term>,
}

impl ActualTerm
{
	fn new(op: Op, children: Vec<Term>) -> ActualTerm
	{
		ActualTerm { op, children }
	}
}

/// A distribution of n usizes that sum to this value.
/// (n, sum)
pub struct Sum(usize, usize);
impl rand::distributions::Distribution<Vec<usize>> for Sum
{
	fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> Vec<usize>
	{
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

consign! {
	 /// Factory for terms.
	 let TERMS_ARC = consign(TERM_SIZE) for ActualTerm ;
}

pub struct TermDist
{
	factory: &'static std::sync::RwLock<HConsign<ActualTerm>>,
	subterm_count: usize,
	binding_depth: usize,
}

impl rand::distributions::Distribution<Term> for TermDist
{
	fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> Term
	{
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
			.map(|n| {
				TermDist {
					factory: self.factory.clone(),
					subterm_count: n + 1,
					binding_depth: if o == Op::Lam {
						self.binding_depth + 1
					} else {
						self.binding_depth
					},
				}
				.sample(rng)
			})
			.collect::<Vec<_>>();
		self.factory.mk(ActualTerm::new(o, subterms))
	}
}

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use std::{
	collections::VecDeque,
	time::{Duration, Instant},
};

pub fn criterion_benchmark(c: &mut Criterion)
{
	let batch_size = 10;
	let mut group = c.benchmark_group("Benchmark");
	for i in (100..=1001).step_by(100) {
		group.bench_with_input(
			BenchmarkId::new("Arc (System Allocator)", i),
			&i,
			move |b, n| {
				let n = *n;

				b.iter_custom(move |iters| {
					let factory = &TERMS_ARC;
					let rng = &mut rand::thread_rng();
					let mut duration = Duration::new(0, 0);

					let dist = TermDist {
						factory,
						subterm_count: 10,
						binding_depth: 1,
					};
					for _ in 0..iters {
						let mut vec = VecDeque::with_capacity(n);
						for _ in 0..n {
							vec.push_back(dist.sample(rng));
						}

						let start = Instant::now();
						for _ in 0..n {
							for _ in 0..batch_size {
								vec.pop_front();
							}
							for _ in 0..batch_size {
								vec.push_back(dist.sample(rng));
							}
							factory.shrink_to_fit();
						}
						duration += start.elapsed()
					}
					factory.shrink_to_fit();

					duration
				});
			},
		);
	}
	group.finish();
}

criterion_group!(
	name = benches;
	config = Criterion::default().with_plots().sample_size(50);
	targets = criterion_benchmark
);
criterion_main!(benches);
