use criterion::{black_box, criterion_group, criterion_main, Criterion};
use inku::{Color, RGBA};

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("saturate", |b| {
        b.iter(|| Color::<RGBA>::new(black_box(0x11223344)).saturate(black_box(0.1)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
