#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate criterion;
#[macro_use]
extern crate pidgin;
extern crate rand;
extern crate regex;

use criterion::{BenchmarkId, Criterion};
use rand::distributions::Alphanumeric;
use rand::prelude::*;
use regex::Regex;
use std::collections::HashSet;
use std::iter;

fn make_words(count: usize) -> Vec<String> {
    let mut rng = rand::thread_rng();
    let mut seen = HashSet::with_capacity(count);
    let mut v = Vec::with_capacity(count);
    loop {
        let size = rng.gen_range(5..30);
        let s: String = iter::repeat(())
            .map(|()| rng.sample(Alphanumeric))
            .take(size)
            .map(|n| String::from_utf8(vec![n]).unwrap())
            .collect();
        if !seen.contains(&s) {
            seen.insert(s.clone());
            v.push(s);
            if v.len() == count {
                break;
            }
        }
    }
    v
}

lazy_static! {
    static ref WORDS: Vec<String> = make_words(1500);
    static ref GOOD: Vec<String> = WORDS
        .iter()
        .map(String::clone)
        .take(750)
        .collect::<Vec<_>>();
    static ref BAD: Vec<String> = WORDS
        .iter()
        .map(String::clone)
        .skip(750)
        .take(750)
        .collect::<Vec<_>>();
    static ref NAIVE_RX_BOUNDED: Regex = Regex::new(
        &(String::from(r"\A(?:")
            + &GOOD
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<&str>>()
                .join("|")
            + r")\z"),
    )
    .unwrap();
    static ref PIDGIN_RX_BOUNDED: Regex = grammar! {
        TOP => r(r"\A") [&GOOD] r(r"\z")
    }
    .rx()
    .unwrap();
    static ref NAIVE_RX_UNBOUNDED: Regex = Regex::new(
        &GOOD
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<&str>>()
            .join("|")
    )
    .unwrap();
    static ref PIDGIN_RX_UNBOUNDED: Regex = grammar! {
        TOP => [&GOOD]
    }
    .rx()
    .unwrap();
}

fn naive_vs_pidgin(c: &mut Criterion) {
    let mut group = c.benchmark_group("naive_rx_vs_pidgin");

    group.bench_function(BenchmarkId::new("naive_good", "bounded"), |b| {
        b.iter(|| {
            for ref w in GOOD.iter() {
                NAIVE_RX_BOUNDED.is_match(&w);
            }
        })
    });
    group.bench_function(BenchmarkId::new("pidgin_good", "bounded"), |b| {
        b.iter(|| {
            for ref w in GOOD.iter() {
                PIDGIN_RX_BOUNDED.is_match(&w);
            }
        })
    });
    group.bench_function(BenchmarkId::new("naive_bad", "bounded"), |b| {
        b.iter(|| {
            for ref w in BAD.iter() {
                NAIVE_RX_BOUNDED.is_match(&w);
            }
        })
    });
    group.bench_function(BenchmarkId::new("pidgin_bad", "bounded"), |b| {
        b.iter(|| {
            for ref w in BAD.iter() {
                PIDGIN_RX_BOUNDED.is_match(&w);
            }
        })
    });
    group.bench_function(BenchmarkId::new("naive_good", "unbounded"), |b| {
        b.iter(|| {
            for ref w in GOOD.iter() {
                NAIVE_RX_UNBOUNDED.is_match(&w);
            }
        })
    });
    group.bench_function(BenchmarkId::new("pidgin_good", "unbounded"), |b| {
        b.iter(|| {
            for ref w in GOOD.iter() {
                PIDGIN_RX_UNBOUNDED.is_match(&w);
            }
        })
    });
    group.bench_function(BenchmarkId::new("naive_bad", "unbounded"), |b| {
        b.iter(|| {
            for ref w in BAD.iter() {
                NAIVE_RX_UNBOUNDED.is_match(&w);
            }
        })
    });
    group.bench_function(BenchmarkId::new("pidgin_bad", "unbounded"), |b| {
        b.iter(|| {
            for ref w in BAD.iter() {
                PIDGIN_RX_UNBOUNDED.is_match(&w);
            }
        })
    });
}

criterion_group!(benches, naive_vs_pidgin);
criterion_main!(benches);
