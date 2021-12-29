#![feature(generators, generator_trait)]

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::ops::Generator;
use std::ops::GeneratorState;
use std::pin::Pin;

use yokai::algo::generate_permutations_wo_dup;

fn permutation_benchmark(c: &mut Criterion) {
    c.bench_function(
        "generate_permutations_wo_dup(vec![1, 2, 3, 4, 5, 6, 7, 8])",
        |b| {
            b.iter(|| {
                let mut gen = generate_permutations_wo_dup(black_box(vec![1, 2, 3, 4, 5, 6, 7, 8]));
                let mut result = vec![];
                while let GeneratorState::Yielded(out) = Pin::new(&mut gen).resume(()) {
                    result.push(out);
                }
                result
            })
        },
    );
}

criterion_group!(benches, permutation_benchmark);
criterion_main!(benches);
