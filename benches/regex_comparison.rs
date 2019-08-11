#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate criterion;
#[macro_use]
extern crate pidgin;
extern crate rand;
extern crate regex;

use criterion::{Criterion, Fun};
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
        let size = rng.gen_range(5, 30);
        let s: String = iter::repeat(())
            .map(|()| rng.sample(Alphanumeric))
            .take(size)
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
    let naive_good = Fun::new("naive_good_bounded", |b, _| {
        b.iter(|| {
            for ref w in GOOD.iter() {
                NAIVE_RX_BOUNDED.is_match(&w);
            }
        })
    });
    let pidgin_good = Fun::new("pidgin_good_bounded", |b, _| {
        b.iter(|| {
            for ref w in GOOD.iter() {
                PIDGIN_RX_BOUNDED.is_match(&w);
            }
        })
    });
    let naive_bad = Fun::new("naive_bad_bounded", |b, _| {
        b.iter(|| {
            for ref w in BAD.iter() {
                NAIVE_RX_BOUNDED.is_match(&w);
            }
        })
    });
    let pidgin_bad = Fun::new("pidgin_bad_bounded", |b, _| {
        b.iter(|| {
            for ref w in BAD.iter() {
                PIDGIN_RX_BOUNDED.is_match(&w);
            }
        })
    });
    let naive_good_unbounded = Fun::new("naive_good_unbounded", |b, _| {
        b.iter(|| {
            for ref w in GOOD.iter() {
                NAIVE_RX_UNBOUNDED.is_match(&w);
            }
        })
    });
    let pidgin_good_unbounded = Fun::new("pidgin_good_unbounded", |b, _| {
        b.iter(|| {
            for ref w in GOOD.iter() {
                PIDGIN_RX_UNBOUNDED.is_match(&w);
            }
        })
    });
    let naive_bad_unbounded = Fun::new("naive_bad_unbounded", |b, _| {
        b.iter(|| {
            for ref w in BAD.iter() {
                NAIVE_RX_UNBOUNDED.is_match(&w);
            }
        })
    });
    let pidgin_bad_unbounded = Fun::new("pidgin_bad_unbounded", |b, _| {
        b.iter(|| {
            for ref w in BAD.iter() {
                PIDGIN_RX_UNBOUNDED.is_match(&w);
            }
        })
    });

    let functions = vec![
        naive_good,
        naive_bad,
        pidgin_good,
        pidgin_bad,
        naive_good_unbounded,
        naive_bad_unbounded,
        pidgin_good_unbounded,
        pidgin_bad_unbounded,
    ];
    c.bench_functions("naive_rx_vs_pidgin", functions, &20);
}

criterion_group!(benches, naive_vs_pidgin);
criterion_main!(benches);
