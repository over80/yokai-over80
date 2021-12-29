#![feature(generators, generator_trait)]

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::ops::Generator;
use std::ops::GeneratorState;
use std::pin::Pin;

use yokai::algo::*;
use yokai::hasher::{ReversedShiftHashValue, ShiftHasher};

fn permutation_benchmark(c: &mut Criterion) {
    c.bench_function(
        "generate_permutations_wo_dup(vec![1, 2, 3, 4, 5, 6, 7, 8])",
        |b| {
            b.iter(|| {
                let mut data = black_box(vec![1, 2, 3, 4, 5, 6, 7, 8]);
                init_permutations_wo_dup(&mut data);
                let mut result: Vec<u8> = vec![];
                loop {
                    result.push(data.iter().sum());
                    if !update_permutations_wo_dup(&mut data) {
                        break;
                    }
                }
                result
            })
        },
    );
    let hasher = ShiftHasher::new();
    c.bench_function("ShiftHasher.progress", |b| {
        b.iter(|| {
            let mut hv = ReversedShiftHashValue::new();
            for code in black_box([0x30, 0x45, 0x12, 0x20, 0x31, 0x15, 0x03, 0x23u8]) {
                hasher.progress(&mut hv, code);
            }
            hv
        })
    });
}

criterion_group!(benches, permutation_benchmark);
criterion_main!(benches);
