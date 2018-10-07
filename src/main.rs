use std::env;
extern crate pidgin;
use pidgin::Matcher;
use pidgin::{Grammar, Pidgin};
extern crate regex;
use regex::Regex;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    // assert!(matcher.parse(""));
    // println!("{:?}", date.matcher().unwrap());
    // let rx = Regex::new(&p.compile()).unwrap();
    // p.add(&args.iter().map(String::as_str).collect::<Vec<&str>>());
    // println!("{}", p.compile());
}
