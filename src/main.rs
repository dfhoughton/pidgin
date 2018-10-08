use std::env;
extern crate pidgin;
use pidgin::Matcher;
use pidgin::{Grammar, Pidgin};
extern crate regex;
use regex::Regex;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    println!("{}", Pidgin::rx(&args.iter().map(String::as_ref).collect::<Vec<&str>>()));
}
